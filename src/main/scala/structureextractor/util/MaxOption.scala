package structureextractor.util

import scala.collection.TraversableOnce

class MaxOption[+A](seq: TraversableOnce[A]) {
  def maxOption[B >: A](implicit cmp: Ordering[B]): Option[A] =
	  if (seq.isEmpty)
		  None
		else
		  Some(seq.max(cmp))
}

object MaxOption {
	implicit def apply[A](seq: TraversableOnce[A]): MaxOption[A] = new MaxOption(seq)
}