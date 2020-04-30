import scala.collection.mutable
import scala.io.Source

case class LabeledDoc(tokens: Array[String], labels: Array[Option[Int]], labelNames: Array[String])

object LabeledDoc {
	sealed trait ReadState
	case object Unlabeled extends ReadState
	case object Label extends ReadState
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
						currentState = Label
					else {
						tokenBuffer += token
						labelBuffer += None
					}
				case Label =>
					val label = labelMap.getOrElseUpdate(token, labelMap.size)
					currentState = Labeled(label)
				case Labeled(label) =>
					if (token == "❯")
						currentState = Unlabeled
					else if (token != "❳" && token != "❮") {
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
		val result = labels.clone()
		var i = (result.length*coverage).toInt
		while (i < result.length && result(i).nonEmpty) i += 1
		while (i < result.length) {
			result(i) = None
			i += 1
		}
		result
	}
}
