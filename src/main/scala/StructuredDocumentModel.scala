import breeze.linalg._
import breeze.numerics._
import breeze.stats.distributions.{Rand, RandBasis}
import com.typesafe.scalalogging.Logger

import scala.collection.JavaConverters._
import scala.annotation.tailrec
import scala.io.Source
import scala.reflect.ClassTag

import Util.abbreviate

case class Arc[SYM](sym: SYM, target: Int)

case class DocumentLattice[SYM](arcs: Array[List[Arc[SYM]]]) {
	/**
		* If arcs(a)(j) == Arc(s, b) for some j, then there is an arc from node position a to node
		* position b that emits s.
		*
		* NOTE: This is NOT a fully general lattice. For each source/destination pair (a/b above),
		* there must be at most one arc.
	 */

	val numNodes = arcs.length + 1

	val startNode = 0
	val finalNode = arcs.length
	val nodes: Range = 0 to finalNode
	val nonfinalNodes: Range = 0 until finalNode

	/** A map from (source_node, target_node) to the [[Arc]]. Not all key pairs are necessarily
		* populated. But if populated, there is one unique [[Arc]]. */
	val arcMap: Map[(Int, Int), Arc[SYM]] = (
		for (
			source <- nonfinalNodes;
			arc <- arcs(source)
		) yield ((source, arc.target), arc)
	).toMap

	def mkString(sep: String = "\n"): String = {
		val arcStrings =
			for (source <- nonfinalNodes; arc <- arcs(source))
				yield s"S$source -> S${arc.target} [${arc.sym.toString}]"
		arcStrings.mkString(sep)
	}
}

object DocumentLattice {
	val logger = Logger("DocumentLattice")

	def parseRule(ruleString: String): Array[Either[String, Int]] = {
		for (tok ← ruleString.split(" "))
			yield """^R(\d+)$""".r.findAllMatchIn(tok).toList match {
				case List(m) ⇒ Right(m.group(1).toInt)
				case _ ⇒ Left(tok)
			}
	}

	@tailrec
	def matchesAt[A](s: IndexedSeq[A], pattern: List[A], pos: Int): Boolean = {
		if (pattern.isEmpty)
			return true
		if (pos >= s.size || s(pos) != pattern.head)
			false
		else
			matchesAt(s, pattern.tail, pos + 1)
	}
	def findOccurrences(tokens: IndexedSeq[String], pattern: String): IndexedSeq[Int] = {
		val patternTokens = pattern.split(" ").toList
//		println(patternTokens)
		for (pos ← 0 to (tokens.length - patternTokens.length)
			         if matchesAt(tokens, patternTokens, pos))
			yield pos
	}

	def fromStringGrammar(grammar: StringGrammar,
	                      allowThreshold: Double, enforceThreshold: Double, alpha: Double)
	: DocumentLattice[String] = {
		val arcs = Array.fill(grammar.tokens.length) {List.empty[Arc[String]]}
		for ((source, target, str) <- grammar.root.arcs(allowThreshold, enforceThreshold, alpha)
		                                if target - source < grammar.tokens.length // exclude root
    ) {
			arcs(source) ::= Arc(str, target)
		}
		DocumentLattice(arcs)
	}

	def fromSequiturGrammar(tokens: IndexedSeq[String], grammar: SequiturGrammar, minFreq: Int = 2,
	                        mergeThresholdPercentile: Double = 0.75)
	: DocumentLattice[String] = {
		val rules = grammar.rules

		val arcs = Array.fill(tokens.length) {List.empty[Arc[String]]}

		val ruleOccurrences = rules map { _.occurrences }
		val ruleLengths = rules map { _.length }
//		val ruleLengths = ruleStrings map { str => str.count(_ == ' ') + 1 }

		println(rules.map(_.freq).sorted.mkString("\n"))
		val occurrenceCounts = ruleOccurrences.map(_.length).sorted
		val calculatedMergeThreshold = occurrenceCounts(
			(mergeThresholdPercentile*ruleOccurrences.length).toInt)
		val mergeThreshold = max(minFreq, calculatedMergeThreshold)
		println(s"mergeThreshold = $mergeThreshold")

		val bestRuleLen = Array.fill(tokens.length) { 0 }
		val mergeMask = Array.fill(tokens.length) { true }
		for (r <- rules.indices) {
			val occurrences = ruleOccurrences(r)
			if (occurrences.length >= mergeThreshold) {
				val str = rules(r).toString
				val len = ruleLengths(r)
				logger.debug(s"Rule R$r (occ: $occurrences) [$str]")
				for (occIndex: Int ← occurrences)
					for (i <- occIndex until occIndex + len) {
						if (len > bestRuleLen(i)) {
							bestRuleLen(i) = len
//							logger.debug(s"  bestRuleLen($i) = $len")
						}
						mergeMask(i) = false
//						logger.debug(s"  mergeMask($i) = false")
					}
			}
		}

		for (r <- rules.indices) {
//			logger.debug("_________")
//			logger.debug(rule.getRuleString)
			val occurrences = ruleOccurrences(r)
			if (occurrences.length >= minFreq) {
				val str = rules(r).toString
				val len = ruleLengths(r)
				for (occIndex: Int ← occurrences) {
					val qualifies = len >= bestRuleLen(occIndex)
					logger.debug(s"  $qualifies: $occIndex -> ${occIndex+len}: $str")
//					if (mergeMask(occIndex) || occurrences.size >= mergeThreshold)
					if (qualifies)
						arcs(occIndex) ::= Arc(str, occIndex + len)
				}
			}
		}

		// Add single-token arcs whereever we don't have high-confidence multi-token arcs
		for ((token, occIndex) ← tokens.zipWithIndex if mergeMask(occIndex))
			arcs(occIndex) ::= Arc(token, occIndex + 1)

		// Sometimes we remove too many arcs and leave the lattice without a single path from
		// start to end. This can always be fixed by adding in single-token arcs to any un-connected
		// ends of existing arcs.
		var targets = (for (arcsFrom <- arcs; arc <- arcsFrom) yield arc.target).toSet

		for (start <- arcs.indices) {
			if (arcs(start).nonEmpty) {
				var u = start
				while (u > 0 && !targets.contains(u)) {
					logger.debug(s"Adding singleton arc into $u")
					arcs(u - 1) ::= Arc(tokens(u - 1), u)
					targets ++= Set(u)
					u -= 1
				}
			}

			for (arc <- arcs(start)) {
				var t = arc.target
				while (t < tokens.length - 1 && arcs(t).isEmpty) {
					logger.debug(s"Adding singleton arc from $t")
					arcs(t) ::= Arc(tokens(t), t + 1)
					targets ++= Set(t + 1)
					t += 1
				}
			}
		}

		DocumentLattice(arcs)
	}

	def buildVocab[SYM: ClassTag](docs: Seq[DocumentLattice[SYM]]): Vocab[SYM] = {
		val syms =
			for (doc <- docs.toStream; // TODO: Is this doing what I want?
			     arcs <- doc.arcs;
		       arc <- arcs)
			yield arc.sym

		Vocab.build(syms)
	}

	def examples = Array(
		new DocumentLattice[String](
			Array(
				List(
					Arc("a", 1)
				),
				List(
					Arc("b", 2)
				),
			)
		),
		new DocumentLattice[String](
			Array(
				List(
					Arc("a", 1)
				),
				List(
					Arc("a", 2)
				),
				List(
					Arc("b", 3)
				),
				List(
					Arc("c", 4)
				),
			)
		),
		new DocumentLattice[String](
			Array(
				List(
					Arc("%", 1)
				),
				List(
					Arc("a", 2)
				),
				List(
					Arc("%", 3)
				),
				List(
					Arc("b", 4)
				),
			)
		),
		new DocumentLattice[String](
			Array(
				List(
					Arc("b", 1)
				),
				List(
					Arc("b", 2),
					Arc("bba", 4)
				),
				List(
					Arc("b", 3)
				),
				List(
					Arc("a", 4)
				),
				List(
					Arc("a", 5)
				),
			)
		),
	)
}


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
  vocab: Vocab[SYM],
  initCost: DenseVector[Double],
  transCost: DenseMatrix[Double],
  emitCost: DenseMatrix[Double] // should this be a Counter?
) {
	val numStates: Int = transCost.rows

	lazy val stateNames: IndexedSeq[String] = (0 until numStates) map { stateName }

	def stateName(i: Int): String =
		s"[[$i:" + abbreviate(vocab(argmax(emitCost(i, ::))).toString, 10) + "]]"

	// The Greek variables for the intermediate calculations are based on Jurafsky & Martin,
	// Speech & Language Processing.
	// The letters t and u refer to node indexes in the doc lattices, with an arc from t to u.
	// i and j are states at t and u, respectively.

	/**
		* The "forward" algorithm. Computes the joint probability of arriving at a particular node
		* in the lattice (and therefore generating the observed sequence up to that point), and being
		* in a particular state at that time.
		*/
	def forward(doc: DocumentLattice[SYM]): DenseMatrix[Double] = {

		val α = DenseMatrix.fill(doc.numNodes, numStates) {
			Double.NegativeInfinity
		}
		α(0, ::) := initCost.t
		for (t <- doc.nonfinalNodes;
		     i <- 0 until numStates;
		     arc <- doc.arcs(t);
		     u = arc.target;
		     j <- 0 until numStates
    ) {
			val cost = α(t, i) + transCost(i, j) + emitCost(i, vocab(arc.sym))
//			println(s"$t $i -> α($u $j) += $cost (${α(t, i)} + ${transCost(i, j)} + ${emitCost(i, vocab
//			(arc.sym))} [emitCost($i, ${vocab(arc.sym)})])")
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
		*/
	def forwardBackward(doc: DocumentLattice[SYM])
		: (DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double], Double) =
	{
		val α = forward(doc)

		// Marginalize over final states to get probability of generating doc
		val logPdoc = softmax(α(doc.finalNode, ::))
//		println(s"logPdoc = $logPdoc")

		val β = DenseMatrix.fill(doc.numNodes, numStates) { Double.NegativeInfinity }
		β(doc.finalNode, ::) := 0.0
		for (t <- doc.nonfinalNodes.reverse;
		     i <- 0 until numStates;
		     arc <- doc.arcs(t);
		     u = arc.target;
		     j <- 0 until numStates
    ) {
			val cost = transCost(i, j) + emitCost(i, vocab(arc.sym)) + β(u, j)
//			println(s"β($t $i) += $cost -> $u $j")
			β(t, i) = softmax(β(t, i), cost)
		}

		// γ(t, i) = Pr([state at node t = i] | doc)
		val γ = α + β - logPdoc

		(α, β, γ, logPdoc)
	}

	/**
		* Perform one Baum-Welch iteration by re-estimating the model on the provided docs
		* using forward-backward algorithm.
		* @return tuple (new model, mean log likelihood)
		*/
	def reestimate(docs: Seq[DocumentLattice[SYM]]): (StructuredDocumentModel[SYM], Double) = {

		var numDocs = 0
		var sumLogPdoc = 0.0
		val initObs = DenseVector.fill(numStates) { Double.NegativeInfinity }
		val transObs = DenseMatrix.fill(numStates, numStates) { Double.NegativeInfinity }
		val emitObs = DenseMatrix.fill(numStates, vocab.size) { Double.NegativeInfinity }

		for (doc <- docs) {
      val (α, β, γ, logPdoc) = forwardBackward(doc)

//			println(s"α = \n$α")
//			println(s"β = \n$β")
//			println(s"γ = \n$γ")

			numDocs += 1
			sumLogPdoc += logPdoc

			initObs := softmax(initObs, γ(0, ::).t)

			for (t <- doc.nonfinalNodes;
		       arc <- doc.arcs(t);
		       u = arc.target)
			{
				// ξ(t, u, i, j) = Pr(arc(t/i -> u/j) | doc)
				val ξ_tu = DenseMatrix.tabulate(numStates, numStates) { case (i, j) =>
					α(t, i) + transCost(i, j) + emitCost(i, vocab(arc.sym)) + β(u, j) - logPdoc
				}

				transObs := softmax(transObs, ξ_tu)

				val arcObs = DenseVector.tabulate(numStates) { i => γ(t, i) }
				emitObs(::, vocab(arc.sym)) := softmax(emitObs(::, vocab(arc.sym)), arcObs)
			}
		}

		val meanDocEntropy = -sumLogPdoc / numDocs
//		println(s"Mean doc entropy = $meanDocEntropy")

		val newModel = new StructuredDocumentModel[SYM](
			vocab,
			initObs - softmax(initObs),
			transObs(::, *) - softmax(transObs, Axis._1),
			emitObs(::, *) - softmax(emitObs, Axis._1)
		)

		(newModel, meanDocEntropy)
	}

	@tailrec
	final def train(
			               docs: Seq[DocumentLattice[SYM]],
			               strategy: TrainingStrategy = FB,
			               tol: Double = 1e-5,
			               prevEntropies: List[Double] = List.empty[Double]
  ): (StructuredDocumentModel[SYM], List[Double]) = {
		val (newModel, meanEntropy) = strategy match {
			case FB | FBThenViterbi => reestimate(docs)
			case Viterbi => reestimateViterbi(docs)
		}
		val newEntropyList = meanEntropy :: prevEntropies
		prevEntropies match {
			case prevEntropy :: _ =>
				if (abs(prevEntropy - meanEntropy)/prevEntropy < tol) {
					if (strategy == FBThenViterbi)
						newModel.train(docs, Viterbi, tol, newEntropyList)
					else
						(newModel, newEntropyList)
				}
				else {
					if (prevEntropy < meanEntropy) {
						println(s"Warning: entropy increased: $newEntropyList")
						//				(this, prevEntropies)
					}
					newModel.train(docs, strategy, tol, newEntropyList)
				}
			case _ => newModel.train(docs, strategy, tol, newEntropyList)
		}
	}

	def reestimateViterbi(docs: Seq[DocumentLattice[SYM]]): (StructuredDocumentModel[SYM], Double) = {

		var numDocs = 0
		var sumLogPpath = 0.0
		val initObs = DenseVector.fill(numStates) { 1e-15 }
		val transObs = DenseMatrix.fill(numStates, numStates) { 1e-15 }
		val emitObs = DenseMatrix.fill(numStates, vocab.size) { 1e-15 }

		for (doc <- docs) {
			val chart = viterbiChart(doc)

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


	def viterbiChart(doc: DocumentLattice[SYM]): ViterbiChart[SYM] = {
		val bestPrevNode = DenseMatrix.zeros[Int](doc.numNodes, numStates)
		val bestPrevState = DenseMatrix.zeros[Int](doc.numNodes, numStates)
		val bestCost = DenseMatrix.fill[Double](doc.numNodes, numStates) {Double.NegativeInfinity}
		bestCost(0, ::) := initCost.t
		for (node1 <- doc.nonfinalNodes;
		     state1 <- 0 until numStates;
		     arc <- doc.arcs(node1);
		     start_cost = bestCost(node1, state1) + emitCost(state1, vocab(arc.sym));
		     node2 = arc.target;
		     state2 <- 0 until numStates)
		{
//			println(s"$node1 $state1 -> $node2 $state2 @ $start_cost")
			val cost = start_cost + transCost(state1, state2)
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

/**
	* Stores results of Viterbi search.
	*
	* The last arc of best path to doc node index t at state i is from node bestPrevNode(t, i) and
	* state bestPrevState(t, i), and has cost (log prob) bestCost(t, i).
	*/
case class ViterbiChart[SYM](
  model: StructuredDocumentModel[SYM],
  doc: DocumentLattice[SYM],
  bestPrevNode: DenseMatrix[Int],
  bestPrevState: DenseMatrix[Int],
  bestCost: DenseMatrix[Double]
) {

	lazy val finalState: Int = argmax(bestCost(doc.finalNode, ::))

	lazy val bestPath: List[(Int, Int)] = bestPathEndingWith(List((doc.finalNode, finalState)))

	lazy val totalCost: Double = bestCost(doc.finalNode, finalState)

	@tailrec
	private def bestPathEndingWith(subpath: List[(Int, Int)]): List[(Int, Int)] = subpath match {
		case (0, _) :: _ => subpath
		case (node, state) :: _ =>
			bestPathEndingWith((bestPrevNode(node, state), bestPrevState(node, state)) :: subpath)
		case _ => throw new Exception("Bug. Don't call this on empty list.")
	}

	def pathInfo(): String = {
		val transitionStrings =
			for (((t, i), (u, j)) <- bestPath zip bestPath.tail;
		       cost = bestCost(u, j) - bestCost(t, i);
			     arcStr = abbreviate(doc.arcMap(t, u).sym.toString, 20))
				yield
					f"${-cost}%10.1f " +
//					f"$t%4d -> $u%4d " +
					f"$t%4d " +
//					f"${model.stateNames(i)}%17s -> ${model.stateNames(j)}%17s " +
					f"${model.stateNames(i)}%17s " +
					s"'$arcStr'"
		transitionStrings.mkString("\n")
	}

	override def toString: String = {
		val pathSet = bestPath.toSet
		val nonInitialNodes = doc.nodes.tail
		val lines = nonInitialNodes map { node =>
			val cells: Seq[(String, String)] = (0 until model.numStates) map { state =>
//				if (bestCost(node, state) == Double.NegativeInfinity) { // shouldn't happen?
				if (bestCost(node, state) <= -9999) {
					val placeholder = s"$state: (   ,   )       "
					(s"  $placeholder  ", s"  $placeholder  ")
				}
				else {
					val prevNode = bestPrevNode(node, state)
					val prevState = bestPrevState(node, state)
					val desc = f"$state: ($prevNode%3d, $prevState%2d) ${-bestCost(node, state)}%6.1f"
					val emit = doc.arcMap(prevNode, node).sym.toString
					if (pathSet.contains((node, state)))
						(s"/ %${desc.length}.${desc.length}s \\" format emit, s"\\ $desc /")
					else
						(s"  %${desc.length}.${desc.length}s  " format emit, s"  $desc  ")
				}
			}
			val tops = cells map { _._1 }
			val bottoms = cells map { _._2 }
			f"    | ${tops.mkString}\n$node%3d | ${bottoms.mkString}"
		}
		lines.mkString("\n")
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
		// TODO make initial state distribution prefer remaining in the same state?
		val p_init = uniformDist(numStates)
//		val p_trans = uniformDistRows(numStates, numStates)
		val p_trans = randomDistRows(numStates, numStates, randBasis.uniform) * 0.1 + 0.9/numStates
		val p_emit = randomDistRows(numStates, vocab.size, randBasis.uniform) * 0.1 + 0.9/vocab.size
		new StructuredDocumentModel[SYM](vocab, log(p_init), log(p_trans), log(p_emit))
	}

	def multiTest[SYM: ClassTag](numStates: Int, docs: List[DocumentLattice[SYM]]): Unit = {
		var entropies = List.empty[Double]
		val numTests = 10
		for (test <- 0 until numTests) {
			val initialModel = StructuredDocumentModel.randomInitial(
				numStates, DocumentLattice.buildVocab(docs), test)
			val (newModel, entropyLog) = initialModel.train(docs)
			entropies ::= entropyLog.head
			println(s"____________\nTest #$test")
//			println(s"\ninitial model:\n$initialModel")
			println(s"\nfinal model:\n$newModel")
			println(s"\nIterations: ${entropyLog.size}")
			println(s"Entropy log: $entropyLog")
			println()
			if (test == numTests - 1) {
				println(newModel.viterbiChart(docs.head).toString)
			}
		}

		println(s"Entropy range: ${entropies.min} - ${entropies.max}")
	}

	def demo(): Unit = {
		val doc = DocumentLattice.examples(2)
		var model = StructuredDocumentModel.examples(2)
		//		var model = StructuredDocumentModel.randomInitial(
		//			2,
		//			Vocab.fromSymbols(Array("a", "b")))
		println(model)
		for (t <- 0 until 10) {
			println(s"________\nIteration $t")
			val vit = model.viterbiChart(doc)
			println(vit.toString())

			model = model.reestimateViterbi(List(doc))._1
			println(model)
		}
	}

	def problemExample3(): Unit = {
		// (BTW, seems to train OK with just '>' as delimiter, but not '> >'.)
		// Hand-coded model for this toy doc has entropy 45.
		// Training from random initialization uses only 2 states, and has entropy 78.
		val numStates = 4
		val minFreq = 5
		val rawText = """
				> >
				  id 1
				  name aa bb
				> >
				  id 2
				  name bb cc
				> >
				  id 3
				  name cc aa
				> >
				  id 4
				  name aa cc
				> >
				  id 5
				  name cc bb
				> >
				  id 6
				  name bb aa
				> >
				  id 7
				  name aa aa
				> >
		"""
		val tokenize = new Tokenizer
		val tokens = tokenize(rawText.toLowerCase).toArray
		val grammar = SequiturGrammar(tokens)
		println("Warning: this hasn't been tested since refactor on 2020-03-31")

		val doc = DocumentLattice.fromSequiturGrammar(tokens, grammar, minFreq, 0.6)
		println(doc.mkString())

		val docs = List(doc)

		val vocab = DocumentLattice.buildVocab(docs)
		val emit = DenseMatrix.fill(numStates, vocab.size) { 0.001 }
		emit(0, vocab("> > id")) = 0.99
		for (i <- 1 to 7)
			emit(1, vocab(i.toString)) = 0.99
		emit(2, vocab("name")) = 0.99
		for (name <- "aa bb cc".split(" "))
			emit(3, vocab(name)) = 0.99
		val trans = DenseMatrix.fill(numStates, numStates) { 0.001 }
		trans(0, 1) = 0.99
		trans(1, 2) = 0.99
		trans(2, 3) = 0.99
		trans(3, 3) = 0.5
		trans(3, 0) = 0.5
		val myModel = new StructuredDocumentModel[String](
			vocab,
			log(DenseVector(1.0, 0.0, 0.0, 0.0)),
			log(trans(::, *) / sum(trans, Axis._1)),
			log(emit(::, *) / sum(emit, Axis._1))
		)
		println("Hand-coded model:")
		println(myModel.viterbiChart(docs.head).pathInfo())
		println()

		val (myModel1, myModel1Entropy) = myModel.reestimateViterbi(docs)
		println(s"Hand-coded model, plus one viterbi re-estimation (entropy=$myModel1Entropy):")
		println(myModel1.viterbiChart(docs.head).pathInfo())
		println()

		val initialModel = StructuredDocumentModel.randomInitial(
			numStates, DocumentLattice.buildVocab(docs))
//		val initialModel = myModel
		val (newModel, entropyLog) = initialModel.train(docs, FBThenViterbi,1e-5)
		println(s"\nfinal model:\n$newModel")
		println(s"\nIterations: ${entropyLog.size}")
		println(s"Entropy log: $entropyLog")
		println()
//		println(newModel.viterbiChart(docs.head).toString)
		println(newModel.viterbiChart(docs.head).pathInfo())
	}

	def main(args: Array[String]): Unit = {
//		multiTest(args(0).toInt, List(DocumentLattice.examples(2))); return
//		demo(); return
//		problemExample3(); return

		val input = if (args.length > 0) args(0) else "b a n a n a"
		val numStates = if (args.length > 1) args(1).toInt else 5
		val allowThreshold = if (args.length > 2) args(2).toDouble else 0.2
		val enforceThreshold = if (args.length > 3) args(3).toDouble else 0.8
		val alpha = if (args.length > 4) args(4).toDouble else 1.0

		val rawText = if (input(0) == '@') {
			val source = Source.fromFile(input.substring(1))
			try source.getLines.mkString(" ") finally source.close
		}
		else input

		val tokenize = new Tokenizer
		val tokens = tokenize(rawText.toLowerCase).toArray
//		val grammar = SequiturGrammar(tokens)
		val grammar = SequiturGrammar(tokens).toStringGrammar

//		val doc = DocumentLattice.fromSequiturGrammar(tokens, grammar, minFreq, 0.6)
		val doc = DocumentLattice.fromStringGrammar(grammar, allowThreshold, enforceThreshold, alpha)
		println(doc.mkString())
//		return // just test DocumentLattice.fromGrammar

		val docs = List(doc)

		val initialModel = StructuredDocumentModel.randomInitial(
			numStates, DocumentLattice.buildVocab(docs))
		val (newModel, entropyLog) = initialModel.train(docs, FBThenViterbi,1e-5)
		println(s"\nfinal model:\n$newModel")
		println(s"\nIterations: ${entropyLog.size}")
		println(s"Entropy log: $entropyLog")
		println()
//		println(newModel.viterbiChart(docs.head).toString)
		println(newModel.viterbiChart(docs.head).pathInfo())


//		ammonite.Main().run("tokens" → tokens, "text" → text, "doc" → doc)
	}
}

