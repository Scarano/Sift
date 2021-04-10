package structureextractor

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

import breeze.linalg.{DenseVector, sum}

import structureextractor.PCFG.Term

class DataGenerator {

}

object DataGenerator {

	private def repeat(s: String, n: Int): String = Stream.continually(s).take(n).mkString(" ")

	def simpleItems(size: Int, prefixLength: Int, descLength: Int, descVocabSize: Int)
	: PCFG	= {
		val list = ("LIST", 1.0, repeat("ITEM", size))
		val item = ("ITEM", 1.0, "PREFIX DESC")
		val prefix = ("PREFIX", 1.0, (1 to prefixLength).map(k => s"p$k").mkString(" "))
		val desc = ("DESC", 1.0, repeat("DESCWORD", descLength))
		val descDist = DenseVector.tabulate(descVocabSize) { k => Math.pow(k+1, -0.5) }
		descDist /= sum(descDist)
		val descWords = (1 to descVocabSize) map { k => ("DESCWORD", descDist(k-1), s"d$k") }

		PCFG(Vector(list, item, prefix, desc) ++ descWords : _*)
	}
	
	def multiFieldItems(size: Int, fieldParams: (Int, Int)*): PCFG = {
		val list = ("LIST", 1.0, repeat("ITEM", size))
		val item = ("ITEM", 1.0,
		            "item " + (1 to fieldParams.length).map("FIELD" + _.toString).mkString(" "))

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


	def generateLabeledDoc(pcfg: PCFG, includedLabels: Seq[String], labelCoverage: Double,
	                       rand: Random)
	: LabeledDoc = {
		val labelMap = includedLabels.zipWithIndex.toMap
		val tokenBuffer = ArrayBuffer.empty[String]
		val labelBuffer = ArrayBuffer.empty[Option[Int]]
		pcfg.generateTree(rand).walk(()) {
			case (_, Term(s), nonterms) =>
				tokenBuffer += s
				labelBuffer += labelMap.get(nonterms.head.name)
		}
		LabeledDoc(tokenBuffer.toArray, labelBuffer.toArray, includedLabels.toVector, labelCoverage)
	}


	def main(args: Array[String]): Unit = {
		val rand = new Random(args(0).toInt)
//		val pcfg = simpleItems(10, 3, 20, 20)
		val pcfg = multiFieldItems(20, (2, 10), (5, 20), (10, 20))
//		println(pcfg.generate(rand).mkString(" "))
		val leafPathPairs = pcfg.generateTree(rand).leafPathPairs
		val leafParentPairs = for ((Term(s), nonterms) <- leafPathPairs)
			yield s"$s/${nonterms.head.name}"
		println(leafParentPairs.mkString(" "))
	}
}
