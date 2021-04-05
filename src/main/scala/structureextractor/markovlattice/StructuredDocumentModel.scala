package structureextractor.markovlattice

import scala.annotation.tailrec
import scala.collection.mutable
import breeze.linalg._
import breeze.numerics._
import breeze.stats.distributions.{Rand, RandBasis}
import structureextractor.Vocab
import structureextractor.Util.abbreviate


sealed trait TrainingStrategy
case object FB extends TrainingStrategy
case object Viterbi extends TrainingStrategy
case object FBThenViterbi extends TrainingStrategy


/**
	* This is like an HMM, except that instead of generating a sentence, it generates a path
	* through a lattice (what NLP people call a DAG). Each arc in the lattice corresponds to some
	* sub-string in the document. An ordinary HMM generates a path in the trivial lattice which
	* has one arc per token, with no branches. Therefore, this is a generalization of an HMM, and
	* can be used as one, if given such a lattice.
	*
	* It supports Viterbi decoding and Baum-Welch training.
	*
	* When you see "cost", that's an abbreviation of "log probability." Almost everything is a
	* log probability, so arithmetic is in the log semiring:
	*   - Values are "added" by multiplying.
	*   - Values are "multiplied" using logsumexp, which Breeze calls [[softmax]].
	*
	* @param initCost initial "cost" distribution (see above) over states.
	* @param transCost transCost(i, j) is cost of transition from state i to j.
	* @param emitCost emitCost(i, w) is cost of emitting word w from state i.
	*/
class StructuredDocumentModel[SYM](
  val vocab: Vocab[SYM],
  val initCost: DenseVector[Double],
  val transCost: DenseMatrix[Double],
  val emitCost: DenseMatrix[Double]
) {
	val numStates: Int = transCost.rows

	lazy val stateNames: IndexedSeq[String] = (0 until numStates) map { stateName }

	def stateName(i: Int): String =
		s"[[$i:" + abbreviate(vocab(argmax(emitCost(i, ::))).toString, 20) + "]]"

	// The Greek variables for the intermediate calculations are based on Jurafsky & Martin,
	// Speech & Language Processing.
	// The letters t and u refer to node indexes in the doc lattices, with an arc from t to u.
	// i and j are states at t and u, respectively.

	/**
		* The "forward" algorithm. Computes the joint probability of arriving at a particular node
		* in the lattice (and therefore generating the observed sequence up to that point), and being
		* in a particular state at that time.
		* Note that when the doc has labels, the probabilities are additionally conditionalized on
		* the labeling.
		*/
	def forward(doc: DocumentLattice[SYM], arcLengthPenalty: Double = 0.0): DenseMatrix[Double] = {

		val α = DenseMatrix.fill(doc.numNodes, numStates) {
			Double.NegativeInfinity
		}
		α(0, ::) := initCost.t // TODO: This should respect doc labels, in case first token is labeled
		for (t <- doc.nonfinalNodes;
		     arc <- doc.arcs(t);
		     i <- 0 until numStates;
		     u = arc.target;
		     u_label = if (u < doc.labels.length) doc.labels(u) else null;
		     j <- 0 until numStates
    ) {
			val transCost_ij =
				if (u_label == null)
					transCost(i, j)
				else if (j == u_label) // All probability mass goes to the transition to the label state
					0.0
				else
					Double.NegativeInfinity
			val arcCost = emitCost(i, vocab(arc.sym)) + arcLengthPenalty * arc.cost
			val cost = α(t, i) + transCost_ij + arcCost
//			println(s"$t S$i -> α($u S$j) += $cost " +
//					s"(${α(t, i)} + $transCost_ij + ${emitCost(i, vocab(arc.sym))} " +
//						s"[emitCost($i, ${vocab(arc.sym)})])")
			// TODO: make more efficient by doing a single softmax for each node/state pair
			α(u, j) = softmax(α(u, j), cost)
		}

		α
	}

	/**
		* The forward-backward algorithm.
		* @return Tuple:
		*         (α (the forward probabilities),
		*          β (the backward probabilities),
		*          γ (the joint probabilities),
		*          log Pr(doc; θ) (AKA log likelihood))
		* Note that when the doc has labels, all probabilities are additionally conditionalized on
		* the labeling.
		*/
	def forwardBackward(doc: DocumentLattice[SYM], arcLengthPenalty: Double = 0.0)
		: (DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double], Double) =
	{
		val α = forward(doc)

		// Marginalize over final states to get probability of generating doc
		val logPdoc = softmax(α(doc.finalNode, ::))
//		println(s"logPdoc = $logPdoc")

		val β = DenseMatrix.fill(doc.numNodes, numStates) { Double.NegativeInfinity }
		β(doc.finalNode, ::) := 0.0
		for (t <- doc.nonfinalNodes.reverse;
		     arc <- doc.arcs(t);
		     i <- 0 until numStates;
		     u = arc.target;
		     j <- 0 until numStates
    ) {
			val transCost_ij = arc match {
				case LabeledArc(_, _, _, label) if label == i =>
					0.0
				case LabeledArc(_, _, _, _) =>
					Double.NegativeInfinity
				case _ =>
					transCost(i, j)
			}
			val arcCost = emitCost(i, vocab(arc.sym)) + arcLengthPenalty * arc.cost
			val cost = transCost_ij + arcCost + β(u, j)
//			println(s"β($t S$i) += $cost -> $u S$j ($transCost_ij + emitCost)")
			β(t, i) = softmax(β(t, i), cost)
		}

		// γ(t, i) = Pr([state at node t = i] | doc)
		val γ = α + β
		γ(::, *) -= softmax(γ, Axis._1)

		(α, β, γ, logPdoc)
	}

	/**
		* Perform one Baum-Welch iteration by re-estimating the model on the provided docs
		* using forward-backward algorithm.
		* @return tuple (new model, mean log likelihood)
		*/
	def reestimate(docs: Seq[DocumentLattice[SYM]], arcPriorWeight: Double = 0.0,
	               flatStates: Int = 0, flatStateBoost: Double = 0.0)
	: (StructuredDocumentModel[SYM], Double) = {

		var numDocs = 0
		var numNodes = 0
		var sumLogPdoc = 0.0
		val initObs = DenseVector.fill(numStates) { Double.NegativeInfinity }
		val transObs = DenseMatrix.fill(numStates, numStates) { Double.NegativeInfinity }
		val emitObs = DenseMatrix.fill(numStates, vocab.size) { Double.NegativeInfinity }

		for (doc <- docs) {
      val (α, β, γ, logPdoc) = forwardBackward(doc)

//			println(s"α = \n${α(0 to 4, ::)}\n...")
//			println(s"β = \n...\n${β(-5 to -1, ::)}\n")
//			println(s"α = \n...\n${α(-5 to -1, ::)}\n")
//			println(s"β = \n${β(0 to 4, ::)}\n...")
//			println(s"α = \n$α")
//			println(s"β = \n$β")
//			println(s"α * β = \n${α + β}")
//			println(s"γ = \n$γ")
			println(s"log P(doc) = $logPdoc")

			numDocs += 1
			numNodes += doc.numNodes
			sumLogPdoc += logPdoc

			initObs := softmax(initObs, γ(0, ::).t)

			for (t <- doc.nonfinalNodes;
		       arc <- doc.arcs(t);
		       u = arc.target)
			{

				// ξ(t, u, i, j) = Pr(arc(t/i -> u/j) | doc, labels)
				val ξ_tu = DenseMatrix.tabulate(numStates, numStates) { case (i, j) =>
					// TODO - remove redundant computation
					val transCost_ij = arc match {
						case LabeledArc(_, _, _, label) if label == i =>
							0.0
						case LabeledArc(_, _, _, _) =>
							Double.NegativeInfinity
						case _ =>
							transCost(i, j)
					}
					val arcCost = emitCost(i, vocab(arc.sym)) + arcPriorWeight * arc.cost
					α(t, i) + transCost_ij + arcCost + β(u, j) - logPdoc
				}
//				println(s"ξ($t, $u) = \n$ξ_tu")
//				println(s"exp ξ($t, $u) = \n${exp(ξ_tu)}")

				// TODO - optimization opportunity(?): Instead of using softmax to convert back out of
				//  log space on each iteration, just do ordinary addition here, and take the exp of the
				//  sum after the loop.
				transObs := softmax(transObs, ξ_tu)

				// Old version: The emission expectation is the probability of being in state i
				// times the probability of being emitted from state i. The problem is that this doesn't
				// favor long arcs when they're part of cheaper overall paths.
//				val arcObs = γ(t, ::).t + emitCost(::, vocab(arc.sym))

				// Marginalize over destination states j to get total expected observations of this arc
				// at position t and state i.
				val arcObs = softmax(ξ_tu, Axis._1)

				// penalize long arcs
//				arcObs :-= DenseVector.fill(arcObs.length) { arcPriorWeight * (u - t) }

				emitObs(::, vocab(arc.sym)) := softmax(emitObs(::, vocab(arc.sym)), arcObs)
			}
		}

		// Hallucinate some observations and transitions to prevent numerical underflow
		// TODO make this a parameter?
		emitObs := softmax(emitObs, DenseMatrix.fill(emitObs.rows, emitObs.cols) {-20.0})
		transObs := softmax(transObs, DenseMatrix.fill(transObs.rows, transObs.cols) {-20.0})

//		println("emitObs = \n" +
//			(0 until emitObs.cols).map(v => {
//				(0 until emitObs.rows).map( i => {
//					f"${exp(emitObs(i, v))}%15.7f "
//				}).mkString + "  " + vocab(v)
//			}).mkString("\n"))

		val meanDocEntropy = -sumLogPdoc / numDocs
//		println(s"Mean doc entropy = $meanDocEntropy")

		// TODO: replace for loop with matrix slices
		val boost = DenseVector.fill(transObs.rows) { log(flatStateBoost * numNodes / flatStates) }
		val emitSmoother = DenseVector.fill(emitObs.cols) { log(vocab.size.toDouble) }
		for (i <- 0 until flatStates) {
			// Artificially favor "flattened" states
			transObs(::, i) := softmax(transObs(::, i), boost)
	//		println(transObs)

			// Artificially flatten those states' emission distributions.
			val oldState0Sum = softmax(emitObs(0, ::))
			emitObs(0, ::) := softmax(emitObs(0, ::), emitSmoother.t)
			emitObs(0, ::) += oldState0Sum - softmax(emitObs(0, ::)) // TODO: is this the formula I want?
			for (w <- 0 until vocab.size)
				emitObs(0, w) -= 1.0 * (vocab(w).toString.count(_ == ' ') + 0)
	//		println(emitObs)
		}

		val newTransCost = transObs(::, *) - softmax(transObs, Axis._1)
		val newEmitCost = emitObs(::, *) - softmax(emitObs, Axis._1)

		// interpolate between old model and re-estimated model. (This is kind of like a learning
		// rate parameter.)
		val λ = 1.0 // TODO: make this a parameter

		val newModel = new StructuredDocumentModel[SYM](
			vocab,
			initObs - softmax(initObs),
			λ*newTransCost + (1-λ)*transCost,
			λ*newEmitCost + (1-λ)*emitCost
		)

		(newModel, meanDocEntropy)
	}

	@tailrec
	final def train(
			               docs: Seq[DocumentLattice[SYM]],
			               strategy: TrainingStrategy = FB,
			               maxEpochs: Int = 99,
			               tol: Double = 1e-5,
			               arcPriorWeight: Double = 0.0,
			               flatStates: Int = 0,
			               flatStateBoost: Double = 0.0,
			               prevCrossentropies: List[Double] = List.empty[Double]
  ): (StructuredDocumentModel[SYM], List[Double]) = {
		val (newModel, meanCrossentropy) = strategy match {
			case FB | FBThenViterbi => reestimate(docs, arcPriorWeight, flatStates, flatStateBoost)
			case Viterbi => reestimateViterbi(docs, arcPriorWeight)
		}
		val newCrossentropyList = meanCrossentropy :: prevCrossentropies
		if (maxEpochs == 1)
			return (newModel, newCrossentropyList)
		prevCrossentropies match {
			case prevEntropy :: _ =>
				if (abs(1 - prevEntropy / meanCrossentropy) < tol) {
					if (strategy == FBThenViterbi)
						newModel.train(docs, Viterbi, maxEpochs - 1, tol, arcPriorWeight,
						               flatStates, flatStateBoost,
						               newCrossentropyList)
					else
						(newModel, newCrossentropyList)
				}
				else {
//					val newArcPriorWeight = if (prevEntropy > meanCrossentropy) arcPriorWeight
//						else {
//							val newValue = max(0.0, arcPriorWeight - 0.25)
//							println(s"Entropy increased ($prevEntropy -> $meanCrossentropy). " +
//									s"Reducing arc prior weight to $newValue.")
//							newValue
//						}
//					val newArcPriorWeight = max(2.0, arcPriorWeight - .5)
					newModel.train(docs, strategy, maxEpochs - 1, tol, arcPriorWeight,
					               flatStates, flatStateBoost,
					               newCrossentropyList)
				}
			case _ => newModel.train(docs, strategy, maxEpochs - 1, tol, arcPriorWeight,
			                         flatStates, flatStateBoost,
			                         newCrossentropyList)
		}
	}

	def reestimateViterbi(docs: Seq[DocumentLattice[SYM]], arcPriorWeight: Double = 0.0)
	: (StructuredDocumentModel[SYM], Double) = {

		var numDocs = 0
		var sumLogPpath = 0.0
		val initObs = DenseVector.fill(numStates) { 1e-15 }
		val transObs = DenseMatrix.fill(numStates, numStates) { 1e-15 }
		val emitObs = DenseMatrix.fill(numStates, vocab.size) { 1e-15 }

		for (doc <- docs) {
			val chart = viterbiChart(doc, arcPriorWeight)

			numDocs += 1
			sumLogPpath += chart.totalCost

			initObs(chart.bestPath.head._2) += 1.0

			for (((t, i), (u, j)) <- chart.bestPath zip chart.bestPath.tail) {
//				println(s"Viterbi: observing transition from ($t, $i) to ($u, $j)")
		    transObs(i, j) += 1.0
				emitObs(i, vocab(doc.arcMap(t, u).sym)) += 1.0
			}
		}

		val meanPathEntropy = -sumLogPpath / numDocs
		println(s"Mean path entropy = $meanPathEntropy")

		val newModel = new StructuredDocumentModel[SYM](
			vocab,
			log(initObs / sum(initObs)),
			log(transObs(::, *) / sum(transObs, Axis._1)),
			log(emitObs(::, *) / sum(emitObs, Axis._1))
		)

		(newModel, meanPathEntropy)
	}


	def viterbiChart(doc: DocumentLattice[SYM], arcPriorWeight: Double = 0.0)
	: ViterbiChart[SYM] = {
		val bestPrevNode = DenseMatrix.zeros[Int](doc.numNodes, numStates)
		val bestPrevState = DenseMatrix.zeros[Int](doc.numNodes, numStates)
		val bestCost = DenseMatrix.fill[Double](doc.numNodes, numStates) {Double.NegativeInfinity}
		bestCost(0, ::) := initCost.t
		for (node1 <- doc.nonfinalNodes;
		     state1 <- 0 until numStates;
		     arc <- doc.arcs(node1);
		     arcCost = emitCost(state1, vocab(arc.sym)) + arcPriorWeight * arc.cost;
		     startCost = bestCost(node1, state1) + arcCost;
		     node2 = arc.target;
		     state2 <- 0 until numStates)
		{
//			println(s"$node1 $state1 -> $node2 $state2 @ $start_cost")
			val cost = startCost + transCost(state1, state2)
			if (cost > bestCost(node2, state2)) {
				bestCost(node2, state2) = cost
				bestPrevState(node2, state2) = state1
				bestPrevNode(node2, state2) = node1
			}
		}

//		println(s"bestPrevNode = $bestPrevNode")
//		println(s"bestPrevState = $bestPrevState")
//		println(s"bestCost = $bestCost")

		ViterbiChart(this, doc, bestPrevNode, bestPrevState, bestCost)
	}

	def viterbiPath(doc: DocumentLattice[SYM]): List[(Int, Int)] = viterbiChart(doc).bestPath

	override def toString: String = {
		List(
			"initCost = ",
			exp(initCost).toString,
			"transCost (transposed) = ",
			(0 until transCost.cols).map(j => {
				(0 until transCost.rows).map( i => {
					f"${exp(transCost(i, j))}%10.7f " // to match emitCost below
				}).mkString
			}).mkString("\n"),
			"emitCost = ",
			(0 until emitCost.cols).map(v => {
				(0 until emitCost.rows).map( i => {
					f"${exp(emitCost(i, v))}%10.7f "
				}).mkString + "  " + vocab(v)
			}).mkString("\n"),
//			(0 until emitCost.cols).map(v => f"${vocab(v)}%10.10s ").mkString,
//			(0 until emitCost.rows).map(i =>
//				(0 until emitCost.cols).map(v => f"${exp(emitCost(i, v))}%10.7f ").mkString
//			).mkString("\n")
		).mkString("\n")
	}
}


object StructuredDocumentModel {

	def examples = Array(
		new StructuredDocumentModel[String](
			Vocab.fromSymbols(Array("a", "b")),
			log(DenseVector(0.5, 0.5)),
			log(DenseMatrix((0.49, 0.51), (0.48, 0.52))),
			log(DenseMatrix((0.45, 0.55), (0.47, 0.53)))
		),
		new StructuredDocumentModel[String](
			Vocab.fromSymbols(Array("a", "b", "c")),
			log(DenseVector(0.5, 0.5)),
			log(DenseMatrix((0.5, 0.5), (0.5, 0.5))),
			log(DenseMatrix((0.8, 0.1, 0.1), (0.1, 0.45, 0.45)))
		),
		new StructuredDocumentModel[String](
			Vocab.fromSymbols(Array("%", "a", "b")),
			log(DenseVector(0.98, 0.01, 0.01)),
			log(DenseMatrix((0.02, 0.49, 0.49), (0.9, 0.05, 0.05), (0.9, 0.05, 0.05))),
			log(DenseMatrix((0.8, 0.1, 0.1), (0.1, 0.8, 0.1), (0.1, 0.1, 0.8)))
		),
		new StructuredDocumentModel[String](
			Vocab.fromSymbols(Array("a", "b", "bba")),
			-DenseVector(1.0, 1.0),
			-DenseMatrix((1.0, 3.0), (3.0, 1.0)),
			-DenseMatrix((1.0, 2.0, 1.0), (2.0, 1.0, 10.0))
		),
	)

	def makeLabelArray(labels: IndexedSeq[String], i: Int = 0, seen: Map[String, Int] = Map.empty)
	: Array[String]	= {
		val seen = mutable.LinkedHashSet.empty[String]
		seen ++= labels
		seen.toArray
	}

	def randomDistRows(rows: Int, cols: Int, rand: Rand[Double]): DenseMatrix[Double] = {
		val D = DenseMatrix.rand(rows, cols, rand)
		D(::, *) /= sum(D, Axis._1)
		D
	}
	def randomDist(size: Int, rand: Rand[Double]): DenseVector[Double] = {
		val D = randomDistRows(1, size, rand)
		D(0, ::).t
	}

	def uniformDistRows(rows: Int, cols: Int): DenseMatrix[Double] = {
		val p = 1.0 / cols
		DenseMatrix.fill(rows, cols) { p }
	}
	def uniformDist(size: Int): DenseVector[Double] =
		uniformDistRows(1, size)(0, ::).t

	def randomInitial[SYM](numStates: Int, vocab: Vocab[SYM], seed: Int = 123)
		 : StructuredDocumentModel[SYM]	=
	{
		val randBasis = RandBasis.withSeed(seed)
//		val p_init = if (numStates == 1) DenseVector(1.0)
//			else DenseVector.tabulate(numStates) { i =>
//				if (i == 0) .9 else 0.1 / (numStates - 1)
//			}
		val p_init = DenseVector.tabulate(numStates) { _ => 1.0 / numStates }
		val p_trans = randomDistRows(numStates, numStates, randBasis.uniform) * 0.1 + 0.9/numStates
		val p_emit = randomDistRows(numStates, vocab.size, randBasis.uniform) * 0.1 + 0.9/vocab.size
		new StructuredDocumentModel[SYM](vocab, log(p_init), log(p_trans), log(p_emit))
	}

}

