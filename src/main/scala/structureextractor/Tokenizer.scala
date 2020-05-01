package structureextractor

import scala.util.matching.Regex

class Tokenizer(preserveWhitespace: Boolean = false, breakOutDigits: Boolean = false) {

	val wordPattern: String = if (breakOutDigits) """\d|""" else ""
	val regex: Regex =
		if (preserveWhitespace) {
			if (breakOutDigits)
				"""[^\W\d]+|\d|\W""".r
			else
				"""\w+|\W""".r
		}
		else {
			if (breakOutDigits)
				"""[^\W\d]+|\d|\S""".r
			else
				"""\w+|\S""".r
		}

	def tokenize(s: String): Iterator[String] = regex.findAllIn(s)

	def apply(s: String): Iterator[String] = tokenize(s)
}

object Tokenizer {
	// TODO: Turn this into a unit test
	def demo(s: String = "'foo123', said I. 45.67baz"): List[String] =
		for (pw <- List(false, true);
		     bod <- List(false, true)) yield {
			val tokens = new Tokenizer(pw, bod)(s)
			val tokenized = tokens.map(s => s"[$s]").mkString(" ")
			s"preserveWhilespace=$pw; breakOutDigits=$bod: $tokenized"
		}

	def main(args: Array[String]): Unit = {
		println(demo().mkString("\n"))
	}
}
