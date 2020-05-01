package structureextractor.markovlattice

import com.typesafe.scalalogging.Logger
import structureextractor.Vocab

import scala.reflect.ClassTag

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
