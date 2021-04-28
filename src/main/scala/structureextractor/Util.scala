package structureextractor

object Util {
	def abbreviate(s: String, maxLen: Int, showLength: Boolean = false): String = {
		if (s.length <= maxLen)
			s
		else {
			val gap = if (showLength) s"[…${s.length}…]" else "…"
			val split = (maxLen - gap.length) / 2
			s.substring(0, split) + gap + s.substring(s.length - (maxLen - split) + gap.length)
		}
	}

	def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit) {
	  val p = new java.io.PrintWriter(f)
	  try { op(p) } finally { p.close() }
	}

	def time[R](f: => R): (R, Long) = {
    val t0 = System.nanoTime()
		val result = f
		(result, System.nanoTime() - t0)
	}
	def timeToStdout[R](f: => R): R = {
		val (result, dur) = time(f)
    println(s"Elapsed time: ${dur/1000000.0} ms")
    result
	}
}
