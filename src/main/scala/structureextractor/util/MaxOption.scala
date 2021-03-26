package structureextractor.util

class MaxOption[+A](seq: Iterable[A]) {
  def maxOption[B >: A](implicit cmp: Ordering[B]): Option[A] =
	  if (seq.isEmpty)
		  None
		else
		  Some(seq.max(cmp))
}

object MaxOption {
	implicit def apply[A](seq: Iterable[A]): MaxOption[A] = new MaxOption(seq)
}