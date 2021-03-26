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
}
