object Util {
	def abbreviate(s: String, maxLen: Int, ellipsis: String = "…"): String = {
		if (s.length <= maxLen)
			s
		else {
			val split = maxLen / 2
			s.substring(0, split) + ellipsis + s.substring(s.length - (maxLen - split) + 1)
		}
	}
}
