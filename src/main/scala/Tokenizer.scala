class Tokenizer(preserveWhitespace: Boolean = false) {

	val regex = if (preserveWhitespace) """\w+|\W""".r else """\w+|\S""".r

	def tokenize(s: String): Iterator[String] = regex.findAllIn(s)

	def apply(s: String): Iterator[String] = tokenize(s)
}
