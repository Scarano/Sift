package structureextractor.preprocessing

import scala.collection.immutable.TreeMap
import scala.collection.mutable
import scala.io.Source

import breeze.linalg.{DenseVector, sum}
import breeze.numerics.log
import breeze.plot
import breeze.plot.{DomainFunction, Figure}

import structureextractor.markovlattice.{AArc, Arc, DocumentLattice, LabeledArc}


class FrequencyScorer(
    val freqs: DenseVector[Int],
    val freqCounts: DenseVector[Double],
) {
	val freqMap: TreeMap[Int, Int] = TreeMap.from(freqs.iterator.map { case (i, freq) => (freq, i) })

	def apply(freq: Int): Double = freqMap.get(freq)
	                                      .map(freqCounts(_))
	                                      .getOrElse(1e-9)

	def interpolatedScore(freq: Int): Double = freqMap.minAfter(freq) match {
		case Some((hiFreq, j)) =>
			if (hiFreq == freq) {
				// Found exact frequency match. Return correponding count.
				freqCounts(j)
			} else freqMap.maxBefore(freq) match {
				case Some((loFreq, i)) =>
					// No exact match. Interpolate between 2 neighboring frequency entries.
					(freqCounts(i) * (hiFreq - freq) + freqCounts(j) * (freq - loFreq)) / (hiFreq - loFreq)
				case _ => 1e-9
			}
		case _ => 1e-9
	}

	def describe: String = {
		val buf = new StringBuilder()

		val sortedFreqCounts = (freqs.valuesIterator zip freqCounts.valuesIterator).toArray
		sortedFreqCounts.sortInPlaceBy(-_._2)

		for (((freq, count), i) <- sortedFreqCounts.take(5).zipWithIndex)
			buf ++= f"#$i: $freq [$count]\n"

		for (q <- 1 to 4; i = q*freqs.size/5 if i >= 5)
			buf ++= f"#$i: ${freqs(i)} [${freqCounts(i)}]\n"

		buf.toString
	}
}

class FrequencyCounter(
    val nGramSize: Int,
    val minCount: Int
) {
	import FrequencyCounter._

	def frequencyScorer(tokens: Iterator[String]): FrequencyScorer = {
		val (freqs, freqScores) = frequencyScores(tokens)
		new FrequencyScorer(freqs, freqScores)
	}

	def frequencyScores(tokens: Iterator[String]): (DenseVector[Int], DenseVector[Double]) = {
		val (freqs, freqCounts) = frequencyCounts(tokens)
//		(freqs, log(freqCounts) - log(sum(freqCounts)))
		(freqs, freqCounts / sum(freqCounts))
	}

	def frequencyCounts(tokens: Iterator[String]): (DenseVector[Int], DenseVector[Double]) = {
		val ngramCounts = countNGrams(tokens, nGramSize).values
		val freqHist = ngramCounts.foldLeft(Map.empty[Int, Int]) { (acc, c) =>
			acc + (c -> (acc.getOrElse(c, 0) + 1))
		}
		val freqHistPairs = freqHist.toArray.sorted.dropWhile(_._1 < minCount)

		val freqs = DenseVector(freqHistPairs.map(_._1))
		val freqCounts = DenseVector(freqHistPairs.map(_._2.toDouble))

		(freqs, freqCounts)

//		val freqScores = for (((freq, _), i) <- freqHistPairs.zipWithIndex)
//			                 yield (freq, frequencyModeScore(i, freqHistPairs))
//		println(ngramCounts)
//		println(freqHist)
//		println(freqHistPairs.mkString("Array(", ", ", ")"))
//		println(freqScores.mkString("Array(", ", ", ")"))
//		freqScores.sortBy(-_._2).view.take(maxPoints).toList
	}
}

object FrequencyCounter {
	def countNGrams(tokens: Iterator[String], n: Int): Map[Seq[String], Int] = {
		val nGrams = tokens.sliding(n, 1)
		nGrams.foldLeft(Map.empty[Seq[String], Int]) { (c, w) =>
				c + (w -> (c.getOrElse(w, 0) + 1))
		}
	}
}

class FrequencySegmenter(
    frequencyScorer: FrequencyScorer,
		minScore: Double, maxArcRatio: Int, minArcFreq: Int,
    freqScoreWeight: Double = 1.0
) {

	def arcScore(c_whole: Int, c_lhs: Int, c_lhs_max: Int, c_rhs: Int, c_rhs_max: Int)
	: Double = {
		val substringScore = 0.001 +
		          SubsequenceFinder.substringScore(c_whole, c_lhs, c_lhs_max, c_rhs, c_rhs_max)
		val freqScore =
			if (c_whole > c_lhs_max && c_whole > c_rhs_max)
				frequencyScorer(c_whole)
			else
				frequencyScorer(0)
//		println(f"arcScore($c_whole, $c_lhs_max, $c_rhs_max) = $freqScore%.5f")

		(1.0 - freqScoreWeight) * log(substringScore) + freqScoreWeight * log(freqScore)
	}

	def makeDocumentLatticeWithDataArcs(tokens: Array[String], labels: IndexedSeq[Option[Int]],
	                                    dataLevels: Int, dataCost: Double,
	                                    truncate: Option[Int] = None)
	: DocumentLattice[String] = {
		val maxArcLen = tokens.length / maxArcRatio

		val finder = new SubsequenceFinder(maxArcLen, minArcFreq, minScore, arcScore)
		val substrings = finder.subsequences(tokens)

		val lastLabeledToken = labels.zipWithIndex.reverse.dropWhile(_._1.isEmpty).headOption.map(_._2)

		val arcs = Array.fill[List[AArc[String]]](truncate.getOrElse(tokens.length)) { Nil }

		println()
		for (ss <- substrings.sortBy(-_.score))
			println(f"[${ss.occurrences.size} occ's from ${ss.start}; ${ss.score}%.2f] "
				        + (ss.start until ss.end).map(tokens(_)).mkString(" "))

		val vertices = mutable.TreeSet(0, arcs.length)

		for (substring <- substrings;
		     ScoredSubstring(start, end, occ, score) = substring;
		     s = "⸬" + tokens.slice(start, end).mkString(" ");
		     t <- occ;
			   u = t + (end - start)
			   if u <= arcs.length
    ) {
			vertices += t
			vertices += u

			// TODO verify arc does not cross label boundary
			arcs(t) ::= (labels(t) match {
				case Some(i) => LabeledArc(s, u, score, i)
				case None if t <= lastLabeledToken.getOrElse(Int.MinValue) + 1 =>
					// Unlabeled token within labeled region; assign the special state -1 to ensure that it
					// gets a non-label state
					LabeledArc(s, u, score, -1)
				case None => Arc(s, u, score)
			})
		}

		val vertexList = vertices.toList
		for (remaining <- vertexList.tails;
		     u <- remaining.slice(1, dataLevels + 1);
		     t = remaining.head
		     if !arcs(t).exists(_.target == u);    // DocumentLattice can't handle duplicate arcs
		     s = tokens.slice(t, u).mkString(" ");
		     cost = -dataCost * (u - t)
		) {
			// TODO verify arc does not cross label boundary
			arcs(t) ::= (labels(t) match {
				case Some(i) => LabeledArc(s, u, cost, i)
				case None if t <= lastLabeledToken.getOrElse(Int.MinValue) + 1 =>
					// Unlabeled token within labeled region; assign the special state -1 to ensure that it
					// gets a non-label state
					LabeledArc(s, u, cost, -1)
				case None => Arc(s, u, cost)
			})
		}

		DocumentLattice(arcs)
	}

	def makeDocumentLattice(tokens: Array[String], labels: IndexedSeq[Option[Int]],
	                        singletonCost: Double = 0.0, truncate: Option[Int] = None)
	: DocumentLattice[String] = {
		val maxArcLen = tokens.length / maxArcRatio

		val finder = new SubsequenceFinder(maxArcLen, minArcFreq, minScore, arcScore)
		val substrings = finder.subsequences(tokens)

		val lastLabeledToken = labels.zipWithIndex.reverse.dropWhile(_._1.isEmpty).headOption.map(_._2)

		val arcs: Array[List[AArc[String]]] =
			Array.tabulate(truncate.getOrElse(tokens.length)) { t =>
				val s = tokens(t)
				List(labels(t) match {
					case Some(i) => LabeledArc(s, t+1, -singletonCost, i)
					case None if t <= lastLabeledToken.getOrElse(Int.MinValue) + 1 =>
						// Unlabeled token within labeled region; assign the special state -1 to ensure that it
						// gets a non-label state
						LabeledArc(s, t+1, -singletonCost, -1)
					case None => Arc(s, t+1, -singletonCost)
				})
			}

		println()
		for (ss <- substrings.sortBy(-_.score))
			println(f"${ss.score}%.2f " + (ss.start until ss.end).map(tokens(_)).mkString(" "))

		for (substring <- substrings;
		     ScoredSubstring(start, end, occ, score) = substring;
		     s = "⸬" + tokens.slice(start, end).mkString(" ");
		     t <- occ;
			   u = t + (end - start)
			   if u <= arcs.length
    ) {
			// TODO verify arc does not cross label boundary
			arcs(t) ::= (labels(t) match {
				case Some(i) => LabeledArc(s, u, score, i)
				case None if t <= lastLabeledToken.getOrElse(Int.MinValue) + 1 =>
					// Unlabeled token within labeled region; assign the special state -1 to ensure that it
					// gets a non-label state
					LabeledArc(s, u, score, -1)
				case None => Arc(s, u, score)
			})
		}
		DocumentLattice(arcs)
	}


}

object FrequencySegmenter {

	def apply(tokens: Iterable[String], nGramSize: Int, minCount: Int, cutoff: Int = Int.MaxValue)
	: FrequencySegmenter = {
		val picker = new FrequencyCounter(nGramSize, minCount)
		val (freqs, freqScores) = picker.frequencyScores(tokens.iterator)

		val sortedFreqScores = (freqs.valuesIterator zip freqScores.valuesIterator)
		                          .toArray.sortBy(-_._2)

		val allowedFreqScores = sortedFreqScores.take(cutoff)
		println(allowedFreqScores.map({ case (f, s) => s"$f: $s" }).mkString("\n"))

		val allowThreshold = log(allowedFreqScores.last._2) - 1e-9 // only works if freqScoreWeight=1.0

		val scorer = picker.frequencyScorer(tokens.iterator)
		println("Frequency counts:")
		println(scorer.describe)
		println()

		new FrequencySegmenter(scorer, allowThreshold, 1, minCount, 1.0)
	}

	def main(args: Array[String]): Unit = {
		val nGramSize = if (args.length >= 2) args(1).toInt else 3
		val cutoff = if (args.length >= 3) args(2).toInt else 5
		val minCount = if (args.length >= 4) args(3).toInt else 2
		val printLimit = if (args.length >= 5) args(4).toInt else Int.MaxValue

		val input = Source.fromFile(args(0))
		val tokens = try {
			val tokenize = new Tokenizer(preserveWhitespace = false, breakOutDigits = false)
			input.getLines.flatMap(tokenize(_)).toList
		}
		finally {
			input.close()
		}

		val segmenter = FrequencySegmenter(tokens, nGramSize, minCount, cutoff)
		val tokenArray = tokens.toArray
		val labels = tokenArray.map { _ => None }
		val doc = segmenter.makeDocumentLattice(tokens.toArray, labels)
		println(doc.mkString(limit=printLimit))

//		scatterPlot(freqs.map(log(_)), freqScores, { i => .01 })
	}

	def scatterPlot[X, Y, V](x: X, y: Y, size: Int => Double)
	                        (implicit xv: DomainFunction[X, Int, V],
	                                  yv: DomainFunction[Y, Int, V],
	                                  vv: V => Double)
	: Unit = {
		val fig = Figure()
		fig.width = 1500
		fig.height = 1000
		val plt = fig.subplot(0)
		plt += plot.scatter(x, y, size)
		fig.refresh()
	}
}
