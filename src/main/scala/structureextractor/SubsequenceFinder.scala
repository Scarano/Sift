package structureextractor

import java.util

import structureextractor.rosettasuffixtree.{Node, SuffixTree}
import structureextractor.util.ResultTree

import collection.JavaConverters._
import scala.collection.mutable


case class ScoredSubstring(start: Int, end: Int, occurrences: Seq[Int], score: Double) {
	def toString(tokens: Seq[String]): String =
		f"[$start -> $end; $score%.3f] " + (start until end).map(tokens(_)).mkString(" ")
}

class SubsequenceFinder(maxLen: Int,
                        minFreq: Int,
                        alpha: Double,
) {

	def substringScore(c_whole: Int, c_lhs: Int, c_rhs: Int): Double =
		c_whole / (math.max(c_lhs, c_rhs) + alpha)

//	def scoredSubstringsFromNodeMut(node: Node[String], parent: Node[String], rhsStart: Int,
//	                                minScore: Double,
//	                                acc: mutable.ListBuffer[ScoredSubstring])
//	: Unit = {
//
//		if (node.getDepth > maxLen)
//			return
//
//		if (node.getDepth > rhsStart && node.getCount >= minFreq) {
//			val c_whole = node.getCount
//			val c_lhs = parent.getCount
//			val c_rhs = node.getTree.lookup(rhsStart, node.getEnd).getCount
//			val score = substringScore(c_whole, c_lhs, c_rhs)
//			if (score > minScore)
//				acc += ScoredSubstring(node.getEnd - node.getDepth, node.getEnd, score)
//		}
//
//		for (child <- node.getChildren.asScala)
//			scoredSubstringsFromNodeMut(child, node, rhsStart, minScore, acc)
//	}

	def scoredSubstringsFromNode(node: Node[String], minScore: Double)
	: ResultTree[ScoredSubstring] = {
		val childResults =
			for (child <- node.getChildren.asScala) yield
				scoredSubstringsFromNode(child, node, child.getLength, minScore)
		ResultTree(Nil, childResults.toList)
	}
	def scoredSubstringsFromNode(node: Node[String], parent: Node[String], rhsOffset: Int,
	                             minScore: Double)
	: ResultTree[ScoredSubstring]	= {

		println("ScoredSubstringsFromNode(" +
				s"${node.getEnd - node.getDepth} -> ${node.getEnd}, " +
				s"${parent.getEnd - parent.getDepth} -> ${parent.getEnd}," +
				s" $rhsOffset")

		if (node.getDepth > maxLen)
			return ResultTree.empty

		if (node.getCount < minFreq)
			return ResultTree.empty

		val rhsStart = node.getEnd - node.getDepth + rhsOffset
		val newSubstring =
			if (node.getDepth <= rhsOffset)
				Nil
			else {
				val c_whole = node.getCount
				val c_lhs = parent.getCount
				val c_rhs = node.getTree.lookup(rhsStart, node.getEnd).getCount
				val score = substringScore(c_whole, c_lhs, c_rhs)
//				println(s"  $c_whole / (max($c_lhs + $c_rhs) + alpha) = $score")
				if (score < minScore)
					Nil
				else
					List(ScoredSubstring(node.getEnd - node.getDepth, node.getEnd,
						                   node.getOccurrences.asScala.toList.map(_.toInt), score))
			}

		val childResults =
			for (child <- node.getChildren.asScala) yield
				scoredSubstringsFromNode(child, node, rhsOffset, minScore)

		ResultTree(newSubstring, childResults.toList)
	}

	def subsequences(tokens: IndexedSeq[String], minScore: Double): Seq[ScoredSubstring] = {

		val sTree = new SuffixTree[String](tokens.asJava)

		scoredSubstringsFromNode(sTree.getRoot, minScore).toList
	}
}

object SubsequenceFinder {

	def main(args: Array[String]): Unit = {
		val string = args.headOption.getOrElse("abcbcbabccbaabcaababcbca")
		val Σ = string.toSet.toVector.sorted
		val maxLen = 4

		val substrings = for (len <- 1 to maxLen; i <- 0 to string.length - maxLen)
			yield string.substring(i, i + len) -> 1

		val counts = substrings.foldLeft(Map.empty[String, Int]) {
			case (m, (s, c)) =>
				m + (s -> (m.getOrElse(s, 0) + c))
		}
		println(" \n \n \n \n \n")

		println(counts.toVector.sortBy(-_._2))

		for ((ch, i) <- string.zipWithIndex) {
			print(s"$ch")
			for (len <- 1 to math.min(maxLen, string.length - i)) {
				print("\t" + counts.getOrElse(string.substring(i, i + len), 0))
			}
			println
		}

		val tokens: IndexedSeq[String] = string.split("")

		val sTree = new SuffixTree[String](tokens.asJava)
		sTree.visualize()

		val finder = new SubsequenceFinder(20, 2, 1.0)
		val subseqs = finder.subsequences(tokens, 0.1)
		for (subseq <- subseqs.sortBy(-_.score))
			println(subseq.toString(tokens))
	}
}
