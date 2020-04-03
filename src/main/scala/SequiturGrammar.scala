import breeze.linalg.{DenseVector, sum}
import breeze.numerics.log
import gi.logic.{GrammarRuleRecord, GrammarRules}
import gi.sequitur.{SAXRule, SequiturFactory}

import scala.annotation.tailrec
import scala.collection.JavaConverters._
import scala.collection.breakOut
import scala.io.Source

import Util.abbreviate


//// abstract string grammar, not necessarily created by sequitur
//// (probably should be moved into its own file)

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

		def mkString(alpha: Double = 1.0, sep: String = "\n", level: Int = 0): String = {
			val indent = "  " * level
			val childStrings = for (child <- children if child.length > 1) yield {
				child.mkString(alpha, sep, level+1)
			}
			f"$indent (${correlationScore(alpha)}%1.2f) ${abbreviate(toString, 60)}" + sep +
				childStrings.mkString(sep)
		}

		// Note: This is recomputed redundantly. But probably not a performance issue?
		def correlationScore(alpha: Double): Double = {
			if (children.isEmpty)
				return 1.0
			val minChildFreq = children.map(_.freq).reduce(Math.min)
			freq / (minChildFreq + alpha)
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
				children.zipWithIndex.flatMap { case (child, i) =>
					child.arcs(allowThreshold, enforceThreshold, alpha, offset + childOffsets(i))
				} (breakOut)

//			println(s"Rule [${abbreviate(toString, 60)}]:")
//			println("  own:   " + ownArcs.map({case (t, u, s) => s"($t, $u)"}).mkString("; "))
//			println("  child: " + childArcs.map({case (t, u, s) => s"($t, $u)"}).mkString("; "))
			ownArcs ++ childArcs
		}
	}
}


//// gi/Sequitur-related stuff:


sealed trait GiRuleElement
case class GiRuleTerminal(string: String) extends GiRuleElement {
	override def toString: String = string
}
case class GiRuleNonterminal(rule: Int) extends GiRuleElement {
	override def toString: String = s"R$rule"
}

case class SequiturGrammar(tokens: IndexedSeq[String], rules: IndexedSeq[SequiturGrammar.Rule]) {
	def toStringGrammar: StringGrammar = {
		val nonterminals = Array.fill[StringGrammar.Rule](rules.length) {null}
		val terminals = scala.collection.mutable.Map.empty[String, StringGrammar.Rule]
		nonterminals(0) = rules(0).toStringGrammarRule(this, nonterminals, terminals)
		StringGrammar(tokens, nonterminals, terminals)
	}
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

		def toStringGrammarRule(grammar: SequiturGrammar,
		                        nonterminals: Array[StringGrammar.Rule],
		                        terminals: scala.collection.mutable.Map[String, StringGrammar.Rule])
		: StringGrammar.Rule	= {
			StringGrammar.Rule(
				children map {
					case GiRuleTerminal(s) =>
						if (!terminals.contains(s)) {
							val occ = findOccurrences(grammar.tokens, s).toArray
							val rule = StringGrammar.Rule(Array.empty, 1, occ, Some(s))
							terminals(s) = rule
						}
						terminals(s)
					case GiRuleNonterminal(rule_id) =>
						if (nonterminals(rule_id) == null) {
							val rule = grammar.rules(rule_id).toStringGrammarRule(grammar, nonterminals, terminals)
							nonterminals(rule_id) = rule
						}
						nonterminals(rule_id)
				},
				length,
				occurrences,
				specifiedString
			)
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

		val sequiturGrammar = SequiturGrammar(tokens, rule.toGrammarRulesData)

		for (rule <- sequiturGrammar.rules) {
			println((rule.stats(sequiturGrammar, tokens.length).productIterator
					      ++ List(rule.toString)).mkString("\t"))
		}

		val grammar = sequiturGrammar.toStringGrammar
		println(grammar.root.mkString(1.0))

	}
}

