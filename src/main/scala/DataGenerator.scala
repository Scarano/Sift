import PCFG.Prod
import breeze.linalg.DenseVector
import breeze.linalg.sum

import scala.util.Random

class DataGenerator {

}

object DataGenerator {

	private def repeat(s: String, n: Int): String =
		Stream.continually(s).take(n).mkString(" ")

	def simpleItems(size: Integer, prefixLength: Integer, descLength: Integer, descVocabSize: Integer)
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

	def main(args: Array[String]): Unit = {
		val rand = new Random(args(0).toInt)
		val pcfg = simpleItems(10, 3, 20, 20)
		println(pcfg.generate(rand).mkString(" "))
	}
}
