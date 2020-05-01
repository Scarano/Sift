package structureextractor

class Compressor {
	def encode(str: String) = {
		val tree = TreeSandbox.buildSuffixTree(str)
		println(tree)

		for ((freq, substr) <- tree.bulkMultipleCommonSubsequence()) {
			println(s"$freq: $substr")
		}

	}
}

object Compressor {
	def main(args: Array[String]): Unit = {
		val text = if (args.length == 1) args(0) else "TOBEORNOTTOBEORTOBEORNOT"
		val comp = new Compressor()
		comp.encode(text)
	}
}