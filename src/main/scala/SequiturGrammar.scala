import SequiturGrammar.Rule
import breeze.linalg.{DenseVector, sum}
import breeze.numerics.log
import gi.logic.{GrammarRuleRecord, GrammarRules}
import gi.sequitur.{SAXRule, SequiturFactory}

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.io.Source

sealed trait GiRuleElement
case class GiRuleTerminal(string: String) extends GiRuleElement {
	override def toString: String = string
}
case class GiRuleNonterminal(rule: Int) extends GiRuleElement {
	override def toString: String = s"R$rule"
}

case class SequiturGrammar(tokens: IndexedSeq[String], rules: IndexedSeq[Rule]) {

}

object SequiturGrammar {

	case class Rule(children: Array[GiRuleElement], length: Int, occurrences: Array[Int],
	                specifiedString: Option[String] = None)
	{
		override lazy val toString: String = specifiedString match {
			case Some(str) => str
			case None => (children map { _.toString }).mkString(" ")
		}
		val freq: Int = occurrences.length

		def stats(grammar: SequiturGrammar, N: Double): (Int, Int, Int, Double, Double, Double) = {
			val elementCounts = children map {
				case GiRuleTerminal(_) => freq  // assume terminals occur only where this rule occurs
				case GiRuleNonterminal(rule) => grammar.rules(rule).freq
			}

			val logN = log(N)
			val minFreq = elementCounts.reduce(Math.min)
			val maxFreq = elementCounts.reduce(Math.max)
			val logP: Double = log(freq.toDouble) - logN
			val logExpected: Double = sum(log(DenseVector(elementCounts))) - elementCounts.length * logN

			(freq, minFreq, maxFreq, logP, logExpected, logP - logExpected)
		}

//		def correlationScore(grammar: SequiturGrammar, delta: Double): Double = {
//			val elementCounts = children map {
//				case GiRuleTerminal(_) => freq  // assume terminals occur only where this rule occurs
//				case GiRuleNonterminal(rule) => grammar.rules(rule).freq
//			}
//			// Log of count(w1) * count(w2) * ...
//			val expJointCount = sum(log(DenseVector(elementCounts)))
//			// Log of count(w1, w2, ...) / [ count(w1) * count(w2) * ... ]
//
//		}
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

	def parseGiRuleString(ruleString: String): Array[GiRuleElement] = {
		for (s <- ruleString.split(" ")) yield {
			if (s.startsWith("R"))
				GiRuleNonterminal(s.substring(1).toInt)
			else
				GiRuleTerminal(s)
		}
	}

	def apply(tokens: IndexedSeq[String], giRules: GrammarRules): SequiturGrammar = {
		val ruleArray: Array[GrammarRuleRecord] = giRules.iterator.asScala.toArray
//		val ruleStrings = ruleArray map { _.getExpandedRuleString.stripSuffix (" ") }
//		val ruleOccurrences = ruleStrings map { findOccurrences(tokens, _) }
//		val ruleFreqs = ruleOccurrences map { _.length }
//		val ruleLengths = ruleStrings map { str => str.count(_ == ' ') + 1 }
//		val giRuleElements = ruleArray map { s => parseGiRuleString(s.getRuleString) }

		val rules = for (giRule <- ruleArray) yield {
			val elements = parseGiRuleString(giRule.getRuleString)
			val str = giRule.getExpandedRuleString.stripSuffix(" ")
			val len = str.count(_ == ' ') + 1
			val occ = findOccurrences(tokens, str)
			if (occ.isEmpty)
				println(s"WARNING: No occurrences found for '$str'")
			Rule(elements, len, occ.toArray, Some(str))
		}

		SequiturGrammar(tokens, rules)
	}

	def apply(tokens: IndexedSeq[String]): SequiturGrammar = {
		val text = tokens.mkString(" ")
		val rule = SequiturFactory.runSequitur(text)
		SequiturGrammar(tokens, rule.toGrammarRulesData)
	}

	def main(args: Array[String]): Unit = {
		val input = if (args.length > 0) args(0) else "b a n a n a"

		val rawText = if (input(0) == '@') {
			val source = Source.fromFile(input.substring(1))
			try source.getLines.mkString(" ") finally source.close
		}
		else input

		val tokenize = new Tokenizer
		val tokens = tokenize(rawText.toLowerCase).toArray
		val text = tokens.mkString(" ")
		val rule = SequiturFactory.runSequitur(text)
		println(SAXRule.printRules)

		val grammar = SequiturGrammar(tokens, rule.toGrammarRulesData)

		for (rule <- grammar.rules) {
			println((rule.stats(grammar, tokens.length).productIterator ++ List(rule.toString)).mkString("\t"))
		}

	}
}

