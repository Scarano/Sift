package structureextractor.markovlattice

import breeze.linalg.{DenseMatrix, DenseVector, Axis, sum, argmax, *}
import breeze.numerics._
import structureextractor.Util.abbreviate

import java.io.{File, PrintWriter}
import scala.annotation.tailrec

/**
	* Stores results of Viterbi search.
	*
	* The last arc of best path to doc node index t at state i is from node bestPrevNode(t, i) and
	* state bestPrevState(t, i), and has cost (log prob) bestCost(t, i).
	*/
case class ViterbiChart[SYM](
		                            model: StructuredDocumentModel[SYM],
		                            doc: DocumentLattice[SYM],
		                            bestPrevNode: DenseMatrix[Int],
		                            bestPrevState: DenseMatrix[Int],
		                            bestCost: DenseMatrix[Double]
                            ) {

	lazy val finalState: Int = argmax(bestCost(doc.finalNode, ::))

	lazy val bestPath: List[(Int, Int)] = bestPathEndingWith(List((doc.finalNode, finalState)))

	lazy val totalCost: Double = bestCost(doc.finalNode, finalState)

	@tailrec
	private def bestPathEndingWith(subpath: List[(Int, Int)]): List[(Int, Int)] = subpath match {
		case (0, _) :: _ => subpath
		case (node, state) :: _ =>
			bestPathEndingWith((bestPrevNode(node, state), bestPrevState(node, state)) :: subpath)
		case _ => throw new Exception("Bug. Don't call this on empty list.")
	}

	def pathInfo(limit: Int = Int.MaxValue, arcLimit: Int = Int.MaxValue): String = {
		val transitionStrings =
			for (((t, i), (u, j)) <- (bestPath zip bestPath.drop(1)).take(limit);
			     cost = bestCost(u, j) - bestCost(t, i);
			     arcStr = abbreviate(doc.arcMap(t, u).sym.toString, arcLimit, showLength = true))
				yield
					f"${-cost}%10.1f " +
//					f"$t%4d -> $u%4d " +
					f"$t%4d " +
//					f"${model.stateNames(i)}%17s -> ${model.stateNames(j)}%17s " +
//					f"${model.stateNames(i)}%17s " +
					f"$i%3d " +
					s"'$arcStr'"
		transitionStrings.mkString("\n")
	}

	def printPath(f: PrintWriter): Unit = {
		for (((t, i), (u, j)) <- (bestPath zip bestPath.drop(1));
		     arcStr = doc.arcMap(t, u).sym.toString)
			f.write(arcStr + "\n")
	}

	/**
		* Create a new version of the [[DocumentLattice]] that has only the arcs used by the
		* Viterbi path.
		*
		* @return new [[DocumentLattice]].
		*/
	def filterArcs(): DocumentLattice[SYM] = {
		val arcs = Array.fill(doc.finalNode) {
			List.empty[AArc[SYM]]
		}
		val pathPositions = bestPath map { _._1 }
		for ((t, u) <- pathPositions zip pathPositions.tail) {
			arcs(t) = List(doc.arcMap(t, u))
		}
		new DocumentLattice[SYM](arcs)
	}

	def stateSummary(out: PrintWriter, maxArcs: Int = 3): Unit = {
		val emitObs = DenseMatrix.fill(model.numStates, model.vocab.size) { 0.0 }
		for (((t, i), (u, _)) <- bestPath zip bestPath.tail) {
			val arc = doc.arcMap(t, u)
			emitObs(i, model.vocab(arc.sym)) += 1.0
		}
		val p = emitObs.copy
		p(::, *) /= sum(p, Axis._1)
		val H = -sum(log(p + 1e-10) *:* p, Axis._1)
		for (i <- 0 until emitObs.rows) {
			out.write(f"State $i (H = ${H(i)}%.2f):\n")
			val obs = emitObs(i, ::).t.valuesIterator.zipWithIndex.toSeq
			val sortedObs = obs.filter(_._1 > 0.0).sorted.reverse
			for ((c, w) <- sortedObs.take(maxArcs))
				out.write(f"  $c%.0f ${model.vocab(w)}\n")
		}
	}

	@tailrec
	final def asRecords(
		firstState: Option[Int],
		path: List[(Int, Int)],
		prevState: Int = -1,
		acc: List[Vector[List[SYM]]] = List.empty)
	: List[Vector[List[SYM]]] = path match {
		case (t, i) :: (u, _) :: _ =>
			val firstState_ = Some(firstState.getOrElse(i))
			val acc2: List[Vector[List[SYM]]] = 
				if ((prevState - firstState_.get) % model.numStates < 0 &&
						(i - firstState_.get) % model.numStates >= 0)
					Vector.fill(model.numStates) { List.empty[SYM] } :: acc
				else
					acc
			val newHeadVec = acc2.head.updated(i, doc.arcMap(t, u).sym :: acc2.head(i))
			val acc3 = newHeadVec :: acc2.tail
			asRecords(firstState_, path.tail, i, acc3)
		case _ =>
			// Rotate fields so that first field comes first
			val state0 = firstState.getOrElse(0)
			acc.reverse.map { rec => rec.drop(state0) ++ rec.take(state0) }
	}

	lazy val records: List[Vector[List[SYM]]] = asRecords(None, bestPath)

	lazy val recordDataColumns: Vector[Int] = records match {
		case Nil => Vector()
		case record0 :: _ =>
			val indexes = for {
				i <- 0 until record0.size
				if (
					for {
						field <- records.view.map(_(i))
						sym <- field
					} yield sym
				).exists(_.toString.startsWith("⸬"))
				// TODO: For the love of God, replace "⸬" with an actual boolean field
			} yield i
			indexes.toVector
	}

	lazy val filteredRecords: List[Vector[List[SYM]]] = 
		records.map( rec => recordDataColumns.map(rec(_)) )

	override def toString: String = {
		val pathSet = bestPath.toSet
		val nonInitialNodes = doc.nodes.tail
		val lines = nonInitialNodes map { node =>
			val cells: Seq[(String, String)] = (0 until model.numStates) map { state =>
//				if (bestCost(node, state) == Double.NegativeInfinity) { // shouldn't happen?
				if (bestCost(node, state) <= -9999) {
					val placeholder = s"$state: (   ,   )       "
					(s"  $placeholder  ", s"  $placeholder  ")
				}
				else {
					val prevNode = bestPrevNode(node, state)
					val prevState = bestPrevState(node, state)
					val desc = f"$state: ($prevNode%3d, $prevState%2d) ${-bestCost(node, state)}%6.1f"
					val emit = doc.arcMap(prevNode, node).sym.toString
					if (pathSet.contains((node, state)))
						(s"/ %${desc.length}.${desc.length}s \\" format emit, s"\\ $desc /")
					else
						(s"  %${desc.length}.${desc.length}s  " format emit, s"  $desc  ")
				}
			}
			val tops = cells map {
				_._1
			}
			val bottoms = cells map {
				_._2
			}
			f"    | ${tops.mkString}\n$node%3d | ${bottoms.mkString}"
		}
		lines.mkString("\n")
	}
}
