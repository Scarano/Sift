package structureextractor

import java.io.File

import scala.io.Source

import structureextractor.Util.printToFile

object Ebay {
	val patterns = List(
		"id" -> """(<span class="item__itemid">Item ID: )(\d+)(</span>)""".r,
		"name" -> """(h3 class="item-title undefined"><a [^>]+><span>)([^<]+)(</span>)""".r,
		"soldon" -> """(<div class="item__sold-date">Sold on: )(\w\w\w \d+)(</div>)""".r,
		"itemprice" -> """(<span class="item__price bold">\$)([.\d]+)(</span>)""".r,
		"totalprice" -> """(<span class="item__order-total bold">\$)([.\d]+)(</span>)""".r,
	)

	def transform1(line: String): String = {
		var result = line
		for ((patternName, pattern) <- patterns) {
			result = pattern.replaceAllIn(result, "$1❲" + patternName + "❳❮❮$2❯❯$3")
		}
		result
	}

	def transform(line: String): String = {
		val transformations =
			for ((patternName, pattern) <- patterns) yield { s: String =>
				pattern.replaceAllIn(s, "$1❲" + patternName + "❳❮$2❯$3")
			}
		transformations.foldLeft(line) { (s, f) => f(s) }
	}

	def main(args: Array[String]): Unit = {
		val inputPath = args(0)
		val outputPath = args(1)

		val source = Source.fromFile(inputPath)
		try printToFile(new File(outputPath)) { writer =>
			for (line <- source.getLines) {
				writer.println(transform(line))
			}
		}
		finally source.close
	}
}
