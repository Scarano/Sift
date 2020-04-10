import PCFG.Prod
import breeze.linalg.DenseVector
import breeze.linalg.sum

import scala.util.Random

class DataGenerator {

}

object DataGenerator {

	private def repeat(s: String, n: Int): String = Stream.continually(s).take(n).mkString(" ")

	def simpleItems(size: Int, prefixLength: Int, descLength: Int, descVocabSize: Int)
	: PCFG	= {
		val list = ("LIST", 1.0, repeat("ITEM", size))
		val item = ("ITEM", 1.0, "\n PREFIX DESC")
		val prefix = ("PREFIX", 1.0, (1 to prefixLength).map(k => s"p$k").mkString(" "))
		val desc = ("DESC", 1.0, repeat("DESCITEM", descLength))
		val descDist = DenseVector.tabulate(descVocabSize) { k => Math.pow(k+1, -0.5) }
		descDist /= sum(descDist)
		val descWords = (1 to descVocabSize) map { k => ("DESCITEM", descDist(k-1), s"d$k") }

		PCFG(Vector(list, item, prefix, desc) ++ descWords : _*)
	}
	
	def multiFieldItems(size: Int, fieldParams: (Int, Int)*): PCFG = {
		val list = ("LIST", 1.0, repeat("ITEM", size))
		val item = ("ITEM", 1.0,
		            "\n " + (1 to fieldParams.length).map("FIELD" + _.toString).mkString(" "))

		val fieldWordPairs: Seq[Vector[(String, Double, String)]] =
			for (((fieldLength, fieldVocabSize), fieldNum) <- fieldParams.zipWithIndex) yield {
			val fieldOrd = fieldNum + 1
			val field = (s"FIELD$fieldOrd", 1.0,
			             s"field$fieldOrd " + repeat(s"FIELD${fieldOrd}WORD", fieldLength))
			val dist = DenseVector.tabulate(fieldVocabSize) { k => Math.pow(k+1, -0.5) }
			dist /= sum(dist)
			val words = (1 to fieldVocabSize) map {
				k => (s"FIELD${fieldOrd}WORD", dist(k-1), s"f${fieldOrd}w$k")
			}
			Vector(field) ++ words
		}

		val prodSpecs = Vector(list, item) ++ fieldWordPairs.flatten

		PCFG(prodSpecs: _*)
	}

	def main(args: Array[String]): Unit = {
		val rand = new Random(args(0).toInt)
//		val pcfg = simpleItems(10, 3, 20, 20)
		val pcfg = multiFieldItems(20, (2, 10), (5, 20), (10, 20))
		println(pcfg.generate(rand).mkString(" "))
	}
}
