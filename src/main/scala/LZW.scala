
// From https://rosettacode.org/wiki/LZW_compression#Scala

object LZW {

	def compress(tc:String) = {
		//initial dictionary
		val startDict = (1 to 255).map(a=>(""+a.toChar,a)).toMap
		val (fullDict, tent_result, remain) = tc.foldLeft ((startDict, List[Int](), "")) {
			case ((dict,res,leftOver),nextChar) =>
				if (dict.contains(leftOver + nextChar)) // current substring already in dict
					(dict, res, leftOver+nextChar)
				else if (dict.size < 4096) // add to dictionary
					(dict + ((leftOver+nextChar, dict.size+1)), dict(leftOver) :: res, ""+nextChar)
				else // dictionary is full
					(dict, dict(leftOver) :: res, ""+nextChar)
		}
		val result = if (remain.isEmpty) tent_result.reverse
		             else (fullDict(remain) :: tent_result).reverse

		val decodeDict = for ((str, code) <- fullDict) yield (code, str)
		for ((code, str) <- decodeDict.toList.sorted if code >= 256) {
			println(s"$code: $str")
		}
		println()
		for (code <- result) {
			if (code < 256)
				print(code.toChar + " ")
			else
				print(s"$code->${decodeDict(code)} ")
		}
		println()
		println()

		result
	}

	def decompress(ns: List[Int]): String = {
		val startDict = (1 to 255).map(a=>(a,""+a.toChar)).toMap
		val (_, result, _) =
			ns.foldLeft[(Map[Int, String], List[String], Option[(Int, String)])]((startDict, Nil, None)) {
				case ((dict, result, conjecture), n) => {
					dict.get(n) match {
						case Some(output) => {
							val (newDict, newCode) = conjecture match {
								case Some((code, prefix)) => ((dict + (code -> (prefix + output.head))), code + 1)
								case None => (dict, dict.size + 1)
							}
							(newDict, output :: result, Some(newCode -> output))
						}
						case None => {
							// conjecture being None would be an encoding error
							val (code, prefix) = conjecture.get
							val output = prefix + prefix.head
							(dict + (code -> output), output :: result, Some(code + 1 -> output))
						}
					}
				}
			}
		result.reverse.mkString("")
	}

	def main(args: Array[String]): Unit = {
		// test
//		val text = "TOBEORNOTTOBEORTOBEORNOT"
		val text = if (args.length == 1) args(0) else "TOBEORNOTTOBEORTOBEORNOT"
		val compressed = compress(text)
		println(compressed)
		val result = decompress(compressed)
		println(result)
	}

}
