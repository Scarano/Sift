package structureextractor.markovlattice

import structureextractor.Util.abbreviate

case class StringGrammar(tokens: IndexedSeq[String],
                         nonterminals: IndexedSeq[StringGrammar.Rule],
                         terminals: collection.mutable.Map[String, StringGrammar.Rule])
{
	def root: StringGrammar.Rule = nonterminals(0)
	def rules: Iterator[StringGrammar.Rule] = nonterminals.iterator ++ terminals.values
}

object StringGrammar {
	// TODO: This should probably be a trait, leaving it up to implementations whether
	// `length` and `occurrences` are specified in the constructor or calculated when needed.
	// TODO: Actually, can I just get rid of length/occurrences by re-writing `arcs`?
	case class Rule(children: Array[Rule], length: Int, occurrences: Array[Int],
	                specifiedString: Option[String] = None)
	{
		val childOffsets: Array[Int] = {
			var offset = 0
			val childOffsets = Array.fill(children.length) {0}
			for (i <- children.indices) {
				childOffsets(i) = offset
				offset += children(i).length
			}
			childOffsets
		}

		def freq: Int = occurrences.length

		override lazy val toString: String = specifiedString match {
			case Some(str) => str
			case None => (children map { _.toString }).mkString(" ")
		}

		def mkString(alpha: Double = 1.0, sep: String = "\n", level: Int = 0,
		             enforceThreshold: Double = 1.0)
		: String	= {
			val indent = "| " * level
			val score = correlationScore(alpha)
			val str = f"$indent(${occurrences.length}; $score%1.2f) ${abbreviate(toString, 100)}"
			if (score >= enforceThreshold) {
				return str
			}
			else {
				val childStrings = for (child <- children if child.length > 1) yield {
					child.mkString(alpha, sep, level+1, enforceThreshold)
				}
				str + sep + childStrings.mkString(sep)
			}
		}

		// Note: This is recomputed redundantly. But probably not a performance issue?
		def correlationScore(alpha: Double = 1.0): Double = {
			if (children.isEmpty)
				return 1.0
			val maxChildFreq = children.map(_.freq).reduce(Math.max)
			freq / (maxChildFreq + alpha)
		}

		def arcs(allowThreshold: Double, enforceThreshold: Double, alpha: Double, offset: Int = 0)
		: Vector[(Int, Int, String)] = {
			val score = correlationScore(alpha)

			val ownArcs: Vector[(Int, Int, String)] = if (score >= allowThreshold)
				Vector((offset, offset + length, toString))
			else
				Vector.empty

			val childArcs: Vector[(Int, Int, String)] = if (score >= enforceThreshold)
				Vector.empty
			else
				children.zipWithIndex.view.flatMap { case (child, i) =>
					child.arcs(allowThreshold, enforceThreshold, alpha, offset + childOffsets(i))
				}.toVector

//			println(s"Rule [${abbreviate(toString, 60)}]:")
//			println("  own:   " + ownArcs.map({case (t, u, s) => s"($t, $u)"}).mkString("; "))
//			println("  child: " + childArcs.map({case (t, u, s) => s"($t, $u)"}).mkString("; "))
			ownArcs ++ childArcs
		}
	}
}