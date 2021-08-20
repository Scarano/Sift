package structureextractor.util

import breeze.generic.UFunc
import breeze.linalg.max
import breeze.linalg.support.CanTraverseValues
import breeze.linalg.support.CanTraverseValues.ValuesVisitor
import breeze.macros.expand
import spire.syntax.cfor._

/**
	* This is a clone of [[breeze.linalg.softmax]] (which computes sums in log space), but with a
	* performance optimization that gives (very slightly) incorrect answers.
	*
  * If x / y > exp(CHEAT_THRESHOLD), then qSoftmax(lx, ly) short-circuits to lx.
  * This is only wrong by a factor of at most exp(-CHEAT_THRESHOLD), and it avoids slow calls to
	* log1p and exp.
	*
	* This may foil gradient-based optimizers. But Baum-Welch doesn't compute gradients.
  */
object qSoftmax extends UFunc {

	val CHEAT_THRESHOLD = 9.0

  implicit object implDoubleDouble extends Impl2[Double, Double, Double] {
    def apply(a: Double, b: Double): Double = {
      if (a.isNegInfinity) return b
      if (b.isNegInfinity) return a
	    val d = a - b
      if (d < 0) {
	      if (d < -CHEAT_THRESHOLD)
		      b
	      else
	        b + scala.math.log1p(scala.math.exp(a - b))
      }
      else {
	      if (d > CHEAT_THRESHOLD)
		      a
	      else
		      a + scala.math.log1p(scala.math.exp(b - a))
      }
    }
  }

  /**
   * Method for computing the max of the first length elements of an array. Arrays
   * of size 0 give Double.NegativeInfinity
   * @param arr
   * @param length
   * @return
   */
  def array(arr: Array[Double], length: Int) = {
    val m = max.array(arr, length)
    if (m.isInfinite) {
      m
    } else {
      var accum = 0.0
      var i = 0
      while (i < length) {
	      val d = arr(i) - m
	      if (d > -CHEAT_THRESHOLD)
	        accum += scala.math.exp(d)
        i += 1
      }
      m + scala.math.log(accum)
    }
  }

  implicit def reduceDouble[T](
      implicit iter: CanTraverseValues[T, Double],
      maxImpl: max.Impl[T, Double]): Impl[T, Double] = new Impl[T, Double] {
    def apply(v: T): Double = {

      val max = if (!iter.isTraversableAgain(v)) 0.0 else maxImpl(v)

      if (max.isInfinite) {
        return Double.NegativeInfinity
      }

      val visit = new ValuesVisitor[Double] {
        var accum = 0.0
        def visit(a: Double): Unit = {
	        val d = a - max
	        if (d > -CHEAT_THRESHOLD)
            accum += scala.math.exp(d)
        }

        def zeros(numZero: Int, zeroValue: Double): Unit = {
          if (numZero != 0) {
            accum += (numZero * scala.math.exp(zeroValue - max))
          }
        }

        override def visitArray(arr: Array[Double], offset: Int, length: Int, stride: Int): Unit = {
          var i = 0
          var off = offset
          while (i < length) {
	          val d = arr(off) - max
	          if (d > -CHEAT_THRESHOLD)
              accum += scala.math.exp(d)
            i += 1
            off += stride
          }

        }
      }

      iter.traverse(v, visit)

      max + scala.math.log(visit.accum)
    }

  }

//  implicit def reduceFloat[T](implicit iter: CanTraverseValues[T, Float], maxImpl: max.Impl[T, Float]): Impl[T, Float] =
//    new Impl[T, Float] {
//      def apply(v: T): Float = {
//
//        val max = if (!iter.isTraversableAgain(v)) 0.0f else maxImpl(v)
//
//        if (max.isInfinite) {
//          return Float.NegativeInfinity
//        }
//
//        val visit = new ValuesVisitor[Float] {
//          var accum = 0.0f
//          def visit(a: Float): Unit = {
//            accum += scala.math.exp(a - max).toFloat
//          }
//
//          def zeros(numZero: Int, zeroValue: Float): Unit = {
//            if (numZero != 0) {
//              accum += (numZero * scala.math.exp(zeroValue - max)).toFloat
//            }
//          }
//
//          override def visitArray(arr: Array[Float], offset: Int, length: Int, stride: Int): Unit = {
//            var i = 0
//            var off = offset
//            var cur = 0.0f
//
//            while (i < length) {
//              cur += scala.math.exp(arr(off) - max).toFloat
//              i += 1
//              off += stride
//            }
//            accum += cur
//          }
//        }
//
//        iter.traverse(v, visit)
//
//        max + scala.math.log(visit.accum).toFloat
//      }
//
//    }
}