package structureextractor

import gstlib.GeneralizedSuffixTreeBuilder
import gstlib.GeneralizedSuffixTreeBuilder.Sequence
import net.seninp.gi.sequitur.{SAXRule, SequiturFactory}

import scala.collection.generic.CanBuildFrom
import scala.io.Source

sealed trait Sym[T]
final case class OriginalSym[T](s: T) extends Sym[T]
case object Terminator extends Sym[Nothing]



class TreeSandbox {
}

object TreeSandbox {
	def buildSuffixTree[Alphabet, Repr <% Sequence[Alphabet]]
			(seq: Repr)
      (implicit icbf: CanBuildFrom[Repr, Alphabet, Repr])
			: GeneralizedSuffixTreeBuilder[Alphabet, Repr] =
	{
		val stree = GeneralizedSuffixTreeBuilder.empty[Alphabet, Repr]()
		stree.insert(seq)
		stree
	}

	def main(args: Array[String]): Unit = {
		val input = if (args.length == 1) args(0) else "b a n a n a"

		val text = if (input(0) == '@') {
			val source = Source.fromFile(input.substring(1))
			try source.getLines.mkString finally source.close
		}
		else input

		val tokenize = new Tokenizer(true)
		val tokens = tokenize(text).toArray

//		val tree = buildSuffixTree(tokens.mkString)
//		println(tree.mkString)

		SequiturFactory.runSequitur(tokens.mkString(" ").toLowerCase)
		println(SAXRule.printRules)

//		val rg = RePairFactory.buildGrammar(text.mkString(" "))
//		println(rg.toGrammarRules)

		val vocab: Vocab[String] = Vocab.build(tokens)
		val symbols = tokens.map(vocab.id_map)

	}
}
