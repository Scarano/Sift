package structureextractor

import org.scalactic.{Equality, TolerantNumerics}
import breeze.linalg.DenseVector
import structureextractor.preprocessing.FrequencyScorer

class FrequencyScorerTest extends org.scalatest.FunSuite {

	implicit val doubleEq: Equality[Double] = TolerantNumerics.tolerantDoubleEquality(1e-9)

	test("FrequencyScorer should interpolate properly") {
		val scorer = new FrequencyScorer(DenseVector(3, 4, 8, 9), DenseVector(3.0, 4.0, 8.0, 9.0))
		val xs = (3 to 9).toList
		val ys = xs.map(scorer(_))
		for ((x, y) <- xs zip ys)
			assert(x === y)
	}
}
