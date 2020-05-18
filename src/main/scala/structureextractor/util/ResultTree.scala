package structureextractor.util

import scala.collection.mutable

class ResultTree[A](results: Seq[A] = Seq.empty, children: List[ResultTree[A]]) {
	def collect(acc: mutable.ListBuffer[A]): Unit = {
		acc ++= results
		for (child <- children)
			child.collect(acc)
	}

	def toList: List[A] = {
		val buf = mutable.ListBuffer.empty[A]
		collect(buf)
		buf.toList
	}
}

object ResultTree {

	def empty[A] = new ResultTree[A](Seq.empty[A], Nil)

	def apply[A](results: Seq[A], children: List[ResultTree[A]]) =
		new ResultTree[A](results, children)

	def leaf[A](results: Seq[A]) = new ResultTree[A](results, Nil)
}

