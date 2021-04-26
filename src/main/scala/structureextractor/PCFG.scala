package structureextractor

import scala.annotation.tailrec
import scala.util.Random

import structureextractor.PCFG._


case class PCFG(prods: Map[String, Seq[Prod]], start: String) {
	// TODO: Reimplement in terms of generateTree?
	def generate(rand: Random): Seq[String] = prods(start).head.generate(this, rand)

	def generateTree(rand: Random): Node = generateTree(start, rand)
	def generateTree(sym: String, rand: Random): Node = prods.get(sym) match {
		case None => Term(sym)
		case Some(prodDist) =>
			val prod = chooseUniform(prodDist, rand.nextDouble())
			Nonterm(sym, prod.rhs.map { generateTree(_, rand) })
	}
}

object PCFG {
	sealed trait Node {
		// TODO: Revisit use cases of this and see if I'm better off using a zipper
		def walk[A](acc: A, ancestors: List[Nonterm] = List.empty)(f: (A, Term, List[Nonterm]) => A): A
		def leafPathPairs: Vector[(Term, List[Nonterm])] = {
			walk(Vector.empty[(Term, List[Nonterm])]) {
				(acc, term, anc) => acc :+ (term, anc)
			}
		}
	}
	case class Term(s: String) extends Node {
		override def toString: String = s

		def walk[A](acc: A, ancestors: List[Nonterm])(f: (A, Term, List[Nonterm]) => A): A = {
			f(acc, this, ancestors)
		}
	}
	case class Nonterm(name: String, children: Seq[Node]) extends Node {
		override def toString: String = s"$name( ${children.mkString(" ")} )"

		override def walk[A](acc: A, ancestors: List[Nonterm])(f: (A, Term, List[Nonterm]) => A): A = {
			val anc1 = this :: ancestors
			children.foldLeft(acc) { (acc1, child) => child.walk(acc1, anc1)(f) }
		}
	}

	@tailrec
	// TODO: This is the lazy, non-efficient implementation of uniform sampling
	private def chooseUniform(options: Seq[Prod], chooseAt: Double): Prod = {
		if (options.tail.isEmpty || chooseAt < options.head.p)
			options.head
		else
			chooseUniform(options.tail, chooseAt - options.head.p)
	}

	case class Prod(lhs: String, p: Double, rhs: Seq[String]) {
		def generate(pcfg: PCFG, rand: Random): Vector[String] = {
			rhs.view.flatMap { x =>
				pcfg.prods.get(x) match {
					case None => Vector(x)
					case Some(prods) => chooseUniform(prods, rand.nextDouble()).generate(pcfg, rand)
				}
			}.to(Vector)
		}
	}

	def apply(prodSpecs: (String, Double, String)*): PCFG = {
		val prods = for ((lhs, p, rhs) <- prodSpecs) yield Prod(lhs, p, rhs.split(" "))
		new PCFG(prods.groupBy(_.lhs), prods.head.lhs)
	}

	val example = PCFG(
		("S", 1.0, "NP VP"),

		("NP", 0.3, "PN"),
		("NP", 0.4, "Det N"),
		("NP", 0.3, "Det N PP"),

		("PN", 0.5, "Alice"),
		("PN", 0.5, "Bob"),

		("Det", 1.0, "the"),

		("N", 0.5, "baker"),
		("N", 0.25, "cobbler"),
		("N", 0.25, "brewer"),

		("PP", 1.0, "P NP"),

		("P", 0.5, "near"),
		("P", 0.5, "across from"),

		("VP", 1.0, "V NP"),

		("V", 0.8, "sees"),
		("V", 0.1, "likes"),
		("V", 0.1, "hates"),
	)

	def main(args: Array[String]): Unit = {
		for (i <- 0 to 19) {
//			println(example.generate(new Random(i)).mkString(" "))
			val tree = example.generateTree(new Random(i))
			println(tree)
			val pairs = tree.walk(Vector.empty[(Term, Nonterm)]) {
				(acc, term, anc) => acc :+ (term, anc.head)
			}
			println(pairs map { case (term, parent) => s"[${parent.name}] $term"} mkString " ")
		}
	}
}


