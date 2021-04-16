package structureextractor.markovlattice

import java.lang.Long
import breeze.linalg.DenseMatrix
import com.typesafe.scalalogging.Logger
import structureextractor.ScoredSubstring
import structureextractor.{SubsequenceFinder, Vocab}

import scala.annotation.tailrec
import scala.reflect.ClassTag
import scala.math.min

abstract class AArc[SYM] {
	val sym: SYM
	val target: Int
	val cost: Double
}
case class Arc[SYM](sym: SYM, target: Int, cost: Double = 0.0) extends AArc[SYM]
case class LabeledArc[SYM](sym: SYM, target: Int, cost: Double = 0.0, label: Int) extends AArc[SYM]

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
		case LabeledArc(_, _, _, label) :: _ => Integer.valueOf(label)
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

	def mkString(sep: String = "\n", limit: Int = Int.MaxValue): String = {
		val arcStrings =
			for (source <- nonfinalNodes.take(limit); arc <- arcs(source))
				yield arc match {
					case Arc(sym, target, cost) =>
						f"$source -> $target $cost%.3f ( ) [${sym.toString}]"
					case LabeledArc(sym, target, cost, label) =>
						f"$source -> $target $cost%.3f ($label) [${sym.toString}]"
				}
		arcStrings.mkString(sep)
	}
}

sealed trait OccurrenceCountTree
case class OccurrenceCountBranch(count: Int, children: Seq[OccurrenceCountTree])
case class OccurrenceCountLeaf(location: Int)
object OccurrenceCountTree {

//	def build(stree: gstlib.InnerTree.NonRootNode): OccurrenceCountTree = stree.
//	def apply(tokens: Seq[String]): OccurrenceCountTree = {
//    val stree = GeneralizedSuffixTreeBuilder.empty[String, Seq[String]]()
//
//    stree.insert(tokens)
//
//		stree.root.
//	}
}

object DocumentLattice {
	val logger = Logger("DocumentLattice")

	/**
		* Divide label slices from [[t]] to [[u]] at label discontinuities.
		* @param labels
		* @param t
		* @param u
		* @return List of pairs [[(v, w)]] that sequentially span [[t]] to [[u]].
		*/
	def labelPartitions(labels: IndexedSeq[Option[Int]], t: Int, u: Int): List[(Int, Int)] =
		labelPartitions(labels, t, u, t, Nil)
	@tailrec
	def labelPartitions(labels: IndexedSeq[Option[Int]], t: Int, u: Int,
	                    start: Int, acc: List[(Int, Int)])
	: List[(Int, Int)] = {
		if (t + 1 == u)
			((start, u) :: acc).reverse
		else if (labels(t) == labels(t + 1))
			labelPartitions(labels, t + 1, u, start, acc)
		else
			labelPartitions(labels, t + 1, u, t + 1, (start, t + 1) :: acc)
	}

	def fromTokens(tokens: Array[String], maxArcLength: Int, labels: IndexedSeq[Option[Int]])
	: DocumentLattice[String] = {

		val arcs: Array[List[AArc[String]]] = Array.tabulate(tokens.length) { start =>
			(
				for (end <- start + 1 to min(start + maxArcLength, tokens.length);
		         s = tokens.slice(start, end).mkString(" ")
			  ) yield {
					// TODO verify arc does not cross label boundary
					labels(start) match {
						case Some(i) => LabeledArc(s, end, -(end - start - 1.0), i)
						case None => Arc(s, end, -(end - start - 1.0))
					}
				}
			).toList
		}

		DocumentLattice(arcs)
	}

	def fromTokensUsingSubsequenceFinder(tokens: Array[String], maxArcRatio: Int, minArcFreq: Int,
	                                     allowThreshold: Double, alpha: Double,
                                       labels: IndexedSeq[Option[Int]])
	: DocumentLattice[String] = {
		val maxArcLen = tokens.length / maxArcRatio

		val finder = new SubsequenceFinder(maxArcLen, minArcFreq, allowThreshold)
		val substrings = finder.subsequences(tokens)

		val arcs: Array[List[AArc[String]]] = Array.tabulate(tokens.length) { t =>
			List(labels(t) match {
				case Some(i) => LabeledArc(tokens(t), t+1, 0.0, i)
				case None => Arc(tokens(t), t+1, 0.0)
			})
		}

		println()
		for (ss <- substrings)
			println(f"${ss.score}%.03f " + (ss.start to ss.end).map(tokens(_)).mkString(" "))

		for (substring <- substrings;
		     ScoredSubstring(start, end, occ, score) = substring;
		     s = tokens.slice(start, end).mkString(" ");
		     t <- substring.occurrences;
			   u = t + (end - start)
    ) {
			// TODO verify arc does not cross label boundary
			arcs(t) ::= (labels(t) match {
				case Some(i) => LabeledArc(s, u, u - t - 1.0, i)
				case None => Arc(s, u, u - t - 1.0)
			})
		}
		DocumentLattice(arcs)
	}

	def fromStringGrammar(grammar: StringGrammar,
	                      allowThreshold: Double, enforceThreshold: Double, alpha: Double)
	: DocumentLattice[String] = {
		val arcs = Array.fill(grammar.tokens.length) {List.empty[AArc[String]]}
		for ((source, target, str) <- grammar.root.arcs(allowThreshold, enforceThreshold, alpha)
		                                if target - source < grammar.tokens.length // exclude root
    ) {
			arcs(source) ::= Arc(str, target, target - source - 1.0)
		}
		DocumentLattice(arcs)
	}

	def fromStringGrammar(grammar: StringGrammar,
	                      allowThreshold: Double, enforceThreshold: Double, alpha: Double,
	                      labels: IndexedSeq[Option[Int]])
	: DocumentLattice[String] = {

		val arcs = Array.fill(grammar.tokens.length) {List.empty[AArc[String]]}
		for ((source, target, str) <- grammar.root.arcs(allowThreshold, enforceThreshold, alpha)
		                                if target - source < grammar.tokens.length; // exclude root
			   (t, u) <- labelPartitions(labels, source, target)
    ) {
			val s = if (t == source && u == target) str else grammar.tokens.slice(t, u).mkString(" ")
			arcs(t) ::= (labels(t) match {
				case Some(i) => LabeledArc(s, u, target - source - 1.0, i)
				case None => Arc(s, u, target - source - 1.0)
			})
		}
		DocumentLattice(arcs)
	}

	def buildVocab[SYM: ClassTag](docs: Seq[DocumentLattice[SYM]],
	                              transform: SYM => SYM = identity[SYM](_)) : Vocab[SYM] = {
		val syms =
			for (doc <- docs.view;
			     arcs <- doc.arcs;
		       arc <- arcs)
			yield arc.sym

		Vocab.build(syms, transform)
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

