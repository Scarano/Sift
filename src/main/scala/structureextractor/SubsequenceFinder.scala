package structureextractor

import scala.jdk.CollectionConverters._

import structureextractor.rosettasuffixtree.{Node, ReversedSuffixTree, SuffixTree}
import structureextractor.util.ResultTree

import SubsequenceFinder.substringScore

case class ScoredSubstring(start: Int, end: Int, occurrences: Seq[Int], score: Double) {
	def toString(tokens: Seq[String]): String =
		f"[$start -> $end; $score%.3f] " + (start until end).map(tokens(_)).mkString(" ")
}

class SubsequenceFinder(maxLen: Int,
                        minFreq: Int,
                        minScore: Double,
                        scoreFn: (Int, Int, Int, Int, Int) => Double = substringScore
) {

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

	def scoredSubstringsFromNode(node: Node[String], revTree: ReversedSuffixTree[String])
	: ResultTree[ScoredSubstring] = {
		val childResults =
			for (child <- node.getChildren.asScala) yield
				scoredSubstringsFromNode(child, node, child.getLength, revTree)
		ResultTree(Nil, childResults.toList)
	}
	def scoredSubstringsFromNode(node: Node[String], parent: Node[String], rhsOffset: Int,
	                             revTree: ReversedSuffixTree[String])
	: ResultTree[ScoredSubstring]	= {

//		println("ScoredSubstringsFromNode(" +
//				s"${node.getEnd - node.getDepth} -> ${node.getEnd}, " +
//				s"${parent.getEnd - parent.getDepth} -> ${parent.getEnd}, " +
//				s"$rhsOffset" +
//		    ")")

		if (node.getDepth > maxLen)
			return ResultTree.empty

		if (node.getCount < minFreq)
			return ResultTree.empty

		val lhsStart = node.getEnd - node.getDepth + 1
		val rhsEnd = node.getEnd - 1
		val newSubstring =
			if (node.getDepth < 2)
				Nil
			else {
				val c_whole = node.getCount
				val c_lhs = node.getTree.lookup(lhsStart, node.getEnd).getCount // TODO can this just use revtree node too?
				val revNode = revTree.lookup(node.getEnd - node.getDepth, node.getEnd)
				val c_lhs_max = if (revNode.getDepth > node.getDepth) revNode.getCount
												else revNode.getChildren.asScala.map(_.getCount).maxOption.getOrElse(0)
//				val c_rhs = parent.getCount // TODO: should only look to parent if length == 1, right?
				val c_rhs = node.getTree.lookup(node.getEnd-node.getDepth, rhsEnd).getCount
				val c_rhs_max = node.getChildren.asScala
						.map(_.getCount).maxOption.getOrElse(0)
				val score = scoreFn(c_whole, c_lhs, c_lhs_max, c_rhs, c_rhs_max)
				if (score < minScore)
					Nil
				else
					List(ScoredSubstring(node.getEnd - node.getDepth, node.getEnd,
						                   node.getOccurrences.asScala.toList.map(_.toInt), score))
			}

		val childResults =
			for (child <- node.getChildren.asScala) yield
				scoredSubstringsFromNode(child, node, rhsOffset, revTree)

		ResultTree(newSubstring, childResults.toList)
	}

	def subsequences(tokens: IndexedSeq[String]): Seq[ScoredSubstring] = {

		val tokenList = tokens.asJava
		val sTree = SuffixTree.ofString(tokenList)
		val revTree = ReversedSuffixTree.ofString(tokenList)
//		sTree.visualize()

		scoredSubstringsFromNode(sTree.getRoot, revTree).toList
	}
}

object SubsequenceFinder {

	def substringScore(c_whole: Int, c_lhs: Int, c_lhs_max: Int, c_rhs: Int, c_rhs_max: Int)
	: Double = {
//		println(s"  substringScore($c_whole, $c_lhs, $c_lhs_max, $c_rhs, $c_rhs_max)")
		val score_lhs = c_whole.toDouble / c_lhs * (1.0 - c_lhs_max.toDouble / c_whole)
		val score_rhs = c_whole.toDouble / c_rhs * (1.0 - c_rhs_max.toDouble / c_whole)
//		println(s"    => min($score_lhs, $score_rhs) = ${math.min(score_lhs, score_rhs)}")
		math.min(score_lhs, score_rhs)
	}

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
		println(counts.toVector.sortBy(-_._2))

		for ((ch, i) <- string.zipWithIndex) {
			print(s"$ch")
			for (len <- 1 to math.min(maxLen, string.length - i)) {
				print("\t" + counts.getOrElse(string.substring(i, i + len), 0))
			}
			println
		}

		val tokens: IndexedSeq[String] = string.split("")

		val sTree = SuffixTree.ofString(tokens.asJava)
		sTree.visualize()

		val finder = new SubsequenceFinder(20, 2, 0.0)
		val subseqs = finder.subsequences(tokens)
		for (subseq <- subseqs.sortBy(-_.score))
			println(subseq.toString(tokens))
	}
}
