import scala.collection.mutable
import scala.io.Source

case class LabeledDoc(tokens: Array[String], labels: Array[Option[Int]], labelNames: Array[String])

object LabeledDoc {
	sealed trait ReadState
	case object Unlabeled extends ReadState
	case class Label(labelTokens: List[String]) extends ReadState
	case class Labeled(label: Int) extends ReadState

	def apply(tokens: Array[String], labels: Array[Option[Int]], labelNames: Array[String],
	          coverage: Double)
	: LabeledDoc = {
		val coveredLabels = reduceLabelCoverage(labels, coverage)
		apply(tokens, coveredLabels, labelNames)
	}

	def apply(text: String, labelCoverage: Double = 1.0): LabeledDoc =
		apply(List(text), labelCoverage)

	def apply(data: TraversableOnce[String], labelCoverage: Double): LabeledDoc = {
		val tokenize = new Tokenizer(preserveWhitespace = false, breakOutDigits = true)
		val rawTokens = data.flatMap(tokenize(_))

		val tokenBuffer = mutable.ArrayBuffer.empty[String]
		val labelBuffer = mutable.ArrayBuffer.empty[Option[Int]]
		val labelMap = mutable.LinkedHashMap.empty[String, Int]

		var currentState: ReadState = Unlabeled
		for (token <- rawTokens) {
			currentState match {
				case Unlabeled =>
					if (token == "❲")
						currentState = Label(Nil)
					else {
						tokenBuffer += token
						labelBuffer += None
					}
				case Label(labelTokens) =>
					if (token == "❳") {
						val labelString = labelTokens.reverse.mkString("")
						val label = labelMap.getOrElseUpdate(labelString, labelMap.size)
						currentState = Labeled(label)
					}
					else
						currentState = Label(token :: labelTokens)
				case Labeled(label) =>
					if (token == "❯")
						currentState = Unlabeled
					else if (token != "❮") {
						tokenBuffer += token
						labelBuffer += Some(label)
					}
			}
		}

		LabeledDoc(tokenBuffer.toArray, labelBuffer.toArray, labelMap.keys.toArray, labelCoverage)
	}

	def apply(source: Source, labelCoverage: Double): LabeledDoc = {
		try apply(source.getLines, labelCoverage) finally source.close
	}

	def reduceLabelCoverage(labels: Array[Option[Int]], coverage: Double): Array[Option[Int]] = {
		if (coverage == 0.0) {
			// unfortunately the condition that prevents us from breaking up labeled regions also
			// requires us to special-case coverage == 0.0
			return Array.fill(labels.length) { None }
		}
		val result = labels.clone()
		var i = Integer.max(1, (result.length*coverage).toInt)
		// Don't break up a contiguous labeled region; skip to next label change
		while (i < result.length && result(i) == result (i - 1)) i += 1
		while (i < result.length) {
			result(i) = None
			i += 1
		}
		result
	}
}
