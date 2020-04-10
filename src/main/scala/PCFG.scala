import PCFG._

import scala.annotation.tailrec
import scala.collection.breakOut
import scala.util.Random


case class PCFG(prods: Map[String, Seq[Prod]], start: String) {
	def generate(rand: Random): Seq[String] = prods(start).head.generate(this, rand)
}

object PCFG {
	sealed trait Node
	case class Term(s: String) extends Node
	case class Nonterm(name: String, children: List[Node]) extends Node

	case class Prod(lhs: String, p: Double, rhs: Seq[String]) {
		@tailrec
		// TODO: This is the lazy, non-efficient implementation of uniform sampling
		private def chooseUniform(options: Seq[Prod], chooseAt: Double): Prod = {
			if (options.tail.isEmpty || chooseAt < options.head.p)
				options.head
			else
				chooseUniform(options.tail, chooseAt - options.head.p)
		}

		def generate(pcfg: PCFG, rand: Random): Vector[String] = {
			rhs.flatMap { x =>
				pcfg.prods.get(x) match {
					case None => Vector(x)
					case Some(prods) => chooseUniform(prods, rand.nextDouble()).generate(pcfg, rand)
				}
			} (breakOut)
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
			println(example.generate(new Random(i)).mkString(" "))
		}
	}
}


