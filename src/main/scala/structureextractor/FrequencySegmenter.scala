package structureextractor

import scala.io.Source
import scala.collection.immutable.TreeMap

import breeze.numerics.log
import breeze.linalg.{DenseVector, linspace, max, softmax, sum}
import breeze.stats.distributions.Gaussian
import breeze.signal.{OptOverhang, convolve}
import breeze.plot
import breeze.plot.{DomainFunction, Figure}

import structureextractor.markovlattice.{AArc, Arc, DocumentLattice, LabeledArc}


class FrequencyScorer(
    val freqs: DenseVector[Int],
    val freqCounts: DenseVector[Double],
) {
	val freqMap: TreeMap[Int, Int] = TreeMap.from(freqs.iterator.map { case (i, freq) => (freq, i) })

	def apply(freq: Int): Double = freqMap.minAfter(freq) match {
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
		var buf = new StringBuilder()

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
	import structureextractor.FrequencyCounter._

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

//		val convSamples = max(10, freqHist.size/20)
//		val convStddev = convSamples/2.0
//		val convKernel = Gaussian(0.0, convStddev)
//				                 .pdf(linspace(-convSamples, convSamples, 2*convSamples+1))
//		val localMeans = convolve(freqCounts, convKernel, overhang=OptOverhang.PreserveLength)
//		val normFreqCounts = freqCounts - localMeans
//
//		//FrequencySegmenter.scatterPlot(log(freqs), log(normFreqCounts), { _ => .01 })
//
//		val freqScores = (freqs.toArray zip normFreqCounts.toArray)
//		                   .slice(convSamples, freqs.size - convSamples)
//		freqScores.sortBy(-_._2).view.take(maxPoints).toList

		(freqs, freqCounts)

//		val freqScores = for (((freq, _), i) <- freqHistPairs.zipWithIndex)
//			                 yield (freq, frequencyModeScore(i, freqHistPairs))
//		println(ngramCounts)
//		println(freqHist)
//		println(freqHistPairs.mkString("Array(", ", ", ")"))
//		println(freqScores.mkString("Array(", ", ", ")"))
//		freqScores.sortBy(-_._2).view.take(maxPoints).toList
	}

//	def frequencyModeScore(freqIndex: Int, freqHist: Seq[(Int, Int)]): Double = {
//		val (freq, freqCount) = freqHist(freqIndex)
//
//		var neighborhoodStart = freqIndex
//		while (neighborhoodStart > 0 && neighborhoodStart > (freqIndex / neighborhoodRatio).toInt)
//			neighborhoodStart -= 1
//		var neighborhoodEnd = freqIndex
//		while (neighborhoodEnd < freqHist.size
//				           && neighborhoodEnd < (freqIndex * neighborhoodRatio).toInt + 1)
//			neighborhoodEnd += 1
//
//		val neighborhood = freqHist.slice(neighborhoodStart, neighborhoodEnd)
//		val nhMean = neighborhood.map(_._2).sum/(neighborhoodEnd - neighborhoodStart)
//
//		(freqCount.toDouble + baselineCount) / (nhMean + baselineCount)
//	}
}

object FrequencyCounter {
//	def countBigrams(tokens: Iterator[String]): Map[(String, String), Int] = {
//		val bigrams = tokens.sliding(2, 1).map { pair => (pair.head, pair.tail.head) }
//		bigrams.foldLeft(Map.empty[(String, String), Int]) { (c, w) =>
//			c + (w -> (c.getOrElse(w, 0) + 1))
//		}
//	}
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
    freqScoreWeight: Double = 0.5
) {

	def arcScore(c_whole: Int, c_lhs: Int, c_lhs_max: Int, c_rhs: Int, c_rhs_max: Int)
	: Double = {
		val substringScore =
		          SubsequenceFinder.substringScore(c_whole, c_lhs, c_lhs_max, c_rhs, c_rhs_max)
		val freqScore = frequencyScorer(c_whole)
//		println(f"$substringScore%.5f freqScore($c_whole) = $freqScore%.5f")
		log((1.0 - freqScoreWeight) * substringScore + freqScoreWeight * freqScore)
	}

	def makeDocumentLattice(tokens: Array[String], labels: IndexedSeq[Option[Int]])
	: DocumentLattice[String] = {
		val maxArcLen = tokens.length / maxArcRatio

		val finder = new SubsequenceFinder(maxArcLen, minArcFreq, minScore, arcScore)
		val substrings = finder.subsequences(tokens)

		val arcs: Array[List[AArc[String]]] = Array.tabulate(tokens.length) { t =>
			List(labels(t) match {
				case Some(i) => LabeledArc(tokens(t), t+1, 0.0, i)
				case None => Arc(tokens(t), t+1, 0.0)
			})
		}

		println()
		for (ss <- substrings.sortBy(-_.score))
			println(f"${ss.score}%.2f " + (ss.start until ss.end).map(tokens(_)).mkString(" "))

		for (substring <- substrings;
		     ScoredSubstring(start, end, occ, score) = substring;
		     s = tokens.slice(start, end).mkString(" ");
		     t <- occ;
			   u = t + (end - start)
    ) {
			// TODO verify arc does not cross label boundary
			arcs(t) ::= (labels(t) match {
				case Some(i) => LabeledArc(s, u, score, i)
				case None => Arc(s, u, score)
			})
		}
		DocumentLattice(arcs)
	}


}

object FrequencySegmenter {

	def main(args: Array[String]): Unit = {
		val nGramSize = if (args.length >= 2) args(1).toInt else 3
		val minCount = if (args.length >= 3) args(2).toInt else 10

		val input = Source.fromFile(args(0))
		val tokens = try {
			val tokenize = new Tokenizer(preserveWhitespace = false, breakOutDigits = true)
			input.getLines.flatMap(tokenize(_)).toList
		}
		finally {
			input.close()
		}

		val picker = new FrequencyCounter(nGramSize, minCount)
		val (freqs, freqScores) = picker.frequencyScores(tokens.iterator)
		println(freqScores)

		scatterPlot(freqs.map(log(_)), freqScores, { i => .01 })
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