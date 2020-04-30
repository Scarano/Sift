import java.io.File

import DataGenerator.simpleItems
import DataGenerator.multiFieldItems
import PCFG.Term
import breeze.linalg._
import breeze.numerics._
import breeze.stats.distributions.{Rand, RandBasis}
import com.typesafe.scalalogging.Logger

import scala.collection.JavaConverters._
import scala.annotation.tailrec
import scala.io.Source
import scala.reflect.ClassTag
import Util.abbreviate

import scala.collection.mutable
import scala.util.Random

abstract class AArc[SYM] {
	val sym: SYM
	val target: Int
}
case class Arc[SYM](sym: SYM, target: Int) extends AArc[SYM]
case class LabeledArc[SYM](sym: SYM, target: Int, label: Int) extends AArc[SYM]

case class DocumentLattice[SYM](arcs: Array[List[AArc[SYM]]]) {
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

	val labels: Array[Integer] = arcs map {
		case LabeledArc(_, _, label) :: _ => Integer.valueOf(label)
		case _ => null
	}

	/** A map from (source_node, target_node) to the [[AArc]]. Not all key pairs are necessarily
		* populated. But if populated, there is one unique [[AArc]]. */
	val arcMap: Map[(Int, Int), AArc[SYM]] = (
		for (
			source <- nonfinalNodes;
			arc <- arcs(source)
		) yield ((source, arc.target), arc)
	).toMap

	def mkString(sep: String = "\n"): String = {
		val arcStrings =
			for (source <- nonfinalNodes; arc <- arcs(source))
				yield arc match {
					case Arc(sym, target) => s"S$source -> S$target ( ) [${sym.toString}]"
					case LabeledArc(sym, target, label) => s"S$source -> S$target ($label) [${sym.toString}]"
				}
		arcStrings.mkString(sep)
	}
}

object DocumentLattice {
	val logger = Logger("DocumentLattice")

	def fromStringGrammar(grammar: StringGrammar,
	                      allowThreshold: Double, enforceThreshold: Double, alpha: Double)
	: DocumentLattice[String] = {
		val arcs = Array.fill(grammar.tokens.length) {List.empty[AArc[String]]}
		for ((source, target, str) <- grammar.root.arcs(allowThreshold, enforceThreshold, alpha)
		                                if target - source < grammar.tokens.length // exclude root
    ) {
			arcs(source) ::= Arc(str, target)
		}
		DocumentLattice(arcs)
	}

	def fromStringGrammarWithLabels(grammar: StringGrammar,
	                                allowThreshold: Double, enforceThreshold: Double, alpha: Double,
	                                labels: IndexedSeq[Option[Int]] = Array.empty[Option[Int]])
	: DocumentLattice[String] = {
		val arcs = Array.fill(grammar.tokens.length) {List.empty[AArc[String]]}
		for ((source, target, str) <- grammar.root.arcs(allowThreshold, enforceThreshold, alpha)
		                                if target - source < grammar.tokens.length // exclude root
    ) {
			if (labels.nonEmpty) {
				if (labels.slice(source + 1, target).forall(_ == labels(source))) {
					arcs(source) ::= (labels(source) match {
						case Some(i) => LabeledArc(str, target, i)
						case None => Arc(str, target)
					})
				}
			}
			else
				arcs(source) ::= Arc(str, target)
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
		s"[[$i:" + abbreviate(vocab(argmax(emitCost(i, ::))).toString, 20) + "]]"

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
			val cost = α(t, i) + transCost_ij + emitCost(i, vocab(arc.sym))
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
		     arc <- doc.arcs(t);
		     i <- 0 until numStates;
		     u = arc.target;
		     j <- 0 until numStates
    ) {
			val transCost_ij = arc match {
				case LabeledArc(_, _, label) if label == i =>
					0.0
				case LabeledArc(_, _, _) =>
					Double.NegativeInfinity
				case _ =>
					transCost(i, j)
			}
			val cost = transCost_ij + emitCost(i, vocab(arc.sym)) + β(u, j)
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
	def reestimate(docs: Seq[DocumentLattice[SYM]]): (StructuredDocumentModel[SYM], Double) = {

		var numDocs = 0
		var sumLogPdoc = 0.0
		val initObs = DenseVector.fill(numStates) { Double.NegativeInfinity }
		val transObs = DenseMatrix.fill(numStates, numStates) { Double.NegativeInfinity }
		val emitObs = DenseMatrix.fill(numStates, vocab.size) { Double.NegativeInfinity }

		for (doc <- docs) {
      val (α, β, γ, logPdoc) = forwardBackward(doc)

//			println(s"α = \n$α")
//			println(s"α / P(doc) = \n${α - logPdoc}")
//			println(s"β = \n$β")
//			println(s"β / P(doc) = \n${β - logPdoc}")
//			println(s"α * β = \n${α + β}")
//			println(s"γ = \n$γ")
//			println(s"log P(doc) = $logPdoc")

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

				// TODO - optimization opportunity(?): Instead of using softmax to convert back out of
				//  log space on each iteration, just do ordinary addition here, and take the exp of the
				//  sum after the loop.
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
			               maxEpochs: Int = 99,
			               tol: Double = 1e-5,
			               prevEntropies: List[Double] = List.empty[Double]
  ): (StructuredDocumentModel[SYM], List[Double]) = {
		val (newModel, meanEntropy) = strategy match {
			case FB | FBThenViterbi => reestimate(docs)
			case Viterbi => reestimateViterbi(docs)
		}
		val newEntropyList = meanEntropy :: prevEntropies
		if (maxEpochs == 1)
			return (newModel, newEntropyList)
		prevEntropies match {
			case prevEntropy :: _ =>
				if (abs(prevEntropy - meanEntropy) < tol) {
					if (strategy == FBThenViterbi)
						newModel.train(docs, Viterbi, maxEpochs - 1, tol, newEntropyList)
					else
						(newModel, newEntropyList)
				}
				else {
					if (prevEntropy < meanEntropy) {
						println(s"Warning: entropy increased: $newEntropyList")
						//				(this, prevEntropies)
					}
					newModel.train(docs, strategy, maxEpochs - 1, tol, newEntropyList)
				}
			case _ => newModel.train(docs, strategy, maxEpochs - 1, tol, newEntropyList)
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
			     arcStr = abbreviate(doc.arcMap(t, u).sym.toString, 80))
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
		// TODO make initial state distribution prefer remaining in the same state?
		val p_init = uniformDist(numStates)
//		val p_trans = uniformDistRows(numStates, numStates)
		val p_trans = randomDistRows(numStates, numStates, randBasis.uniform) * 0.2 + 0.8/numStates
		val p_emit = randomDistRows(numStates, vocab.size, randBasis.uniform) * 0.2 + 0.8/vocab.size
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

	def main(args: Array[String]): Unit = {
//		multiTest(args(0).toInt, List(DocumentLattice.examples(2))); return
//		demo(); return
//		problemExample3(); return

		case class Config(
				                 input: String = "b a n a n a",
				                 inputFile: File = null,
				                 generateSimple: Seq[Int] = Seq(),
				                 generateMulti: Seq[Int] = Seq(),
				                 states: Int = 5,
				                 allowThreshold: Double = 0.2,
				                 enforceThreshold: Double = 0.8,
				                 alpha: Double = 1.0,
				                 labelCoverage: Double = 0.5,
				                 tolerance: Double = 1e-5,
				                 maxEpochs: Int = 99,
		)
		val parser = new scopt.OptionParser[Config]("StructuredDocumentModel") {
			opt[String]("input").action( (x, c) =>
				c.copy(input = x)
			)
			opt[File]("input-file").action( (x, c) =>
				c.copy(inputFile = x)
			)
			opt[Seq[Int]]("generate-simple").action( (x, c) =>
				c.copy(generateSimple = x)
			)
			opt[Seq[Int]]("generate-multi").action( (x, c) =>
				c.copy(generateMulti = x)
			)
			opt[Int]("states").action( (x, c) =>
				c.copy(states = x)
			)
			opt[Double]("allow-threshold").action( (x, c) =>
				c.copy(allowThreshold = x)
			)
			opt[Double]("enforce-threshold").action( (x, c) =>
				c.copy(enforceThreshold = x)
			)
			opt[Double]("alpha").action( (x, c) =>
				c.copy(alpha = x)
			)
			opt[Double]("label-coverage").action( (x, c) =>
				c.copy(labelCoverage = x)
			)
			opt[Double]("tolerance").action( (x, c) =>
				c.copy(tolerance = x)
			)
			opt[Int]("max-epochs").action( (x, c) =>
				c.copy(maxEpochs = x)
			)
		}
		val config = parser.parse(args, Config()) match {
			case Some(c) => c
			case None =>
				throw new Exception("Invalid arguments")
		}

		val labeledDoc = if (config.generateSimple.nonEmpty) {
			if (config.generateSimple.size != 4)
				throw new Exception(s"Wrong format for simpleItems parameters")
			val Seq(size, prefixLength, descLength, descVocabSize) = config.generateSimple
			val pcfg = simpleItems(size, prefixLength, descLength, descVocabSize)
			DataGenerator.generateLabeledDoc(pcfg, List("DESCWORD"), config.labelCoverage, new Random(0))
		}
		else if (config.generateMulti.nonEmpty) {
			if (config.generateMulti.size != 1)
				throw new Exception(s"Wrong format for multiFieldItems parameters")
			val pcfg = DataGenerator.multiFieldItems(config.generateMulti.head,
			                                         (2, 10), (5, 20), (10, 20))
			DataGenerator.generateLabeledDoc(pcfg, List("FIELD1WORD", "FIELD2WORD", "FIELD3WORD"),
			                                 config.labelCoverage, new Random(0))
		}
		else if (config.inputFile != null)
			LabeledDoc(Source.fromFile(config.inputFile), config.labelCoverage)
		else
			LabeledDoc(config.input, config.labelCoverage)

		val grammar = SequiturGrammar(labeledDoc.tokens).toStringGrammar

		val doc = DocumentLattice.fromStringGrammarWithLabels(
		              grammar, config.allowThreshold, config.enforceThreshold, config.alpha,
									labeledDoc.labels)
		println(doc.mkString())
//		return // just test DocumentLattice.fromGrammar

		val docs = List(doc)

		val initialModel = StructuredDocumentModel.randomInitial(
			config.states, DocumentLattice.buildVocab(docs))
		val (newModel, entropyLog) = initialModel.train(docs, FB, config.maxEpochs, config.tolerance)
		println(s"\nfinal model:\n$newModel")
		println(s"\nIterations: ${entropyLog.size}")
		println(s"Entropy log: $entropyLog")
		println()
//		println(newModel.viterbiChart(docs.head).toString)
		println(newModel.viterbiChart(docs.head).pathInfo())


//		ammonite.Main().run("tokens" → tokens, "text" → text, "doc" → doc)
	}
}

