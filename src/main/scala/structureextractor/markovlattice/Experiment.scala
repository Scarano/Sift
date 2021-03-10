package structureextractor.markovlattice

import java.io.File

import structureextractor.{DataGenerator, LabeledDoc}

import scala.io.Source
import scala.reflect.ClassTag
import scala.util.Random

object Experiment {
	def multiTest[SYM: ClassTag](numStates: Int, docs: List[DocumentLattice[SYM]]): Unit = {
		var entropies = List.empty[Double]
		val numTests = 10
		for (test <- 0 until numTests) {
			val initialModel = StructuredDocumentModel.randomInitial(
				numStates, DocumentLattice.buildVocab(docs), test)
			val (newModel, entropyLog) = initialModel.train(docs)
			entropies ::= entropyLog.head
			println(s"____________\nTest #$test")
//			println(s"\ninitial model:\n$initialModel")
			println(s"\nfinal model:\n$newModel")
			println(s"\nIterations: ${entropyLog.size}")
			println(s"Entropy log: $entropyLog")
			println()
			if (test == numTests - 1) {
				println(newModel.viterbiChart(docs.head).toString)
			}
		}

		println(s"Entropy range: ${entropies.min} - ${entropies.max}")
	}

	def demo(): Unit = {
		val doc = DocumentLattice.examples(2)
		var model = StructuredDocumentModel.examples(2)
		//		var model = StructuredDocumentModel.randomInitial(
		//			2,
		//			Vocab.fromSymbols(Array("a", "b")))
		println(model)
		for (t <- 0 until 10) {
			println(s"________\nIteration $t")
			val vit = model.viterbiChart(doc)
			println(vit.toString())

			model = model.reestimateViterbi(List(doc))._1
			println(model)
		}
	}

	def main(args: Array[String]): Unit = {
//		multiTest(args(0).toInt, List(DocumentLattice.examples(2))); return
//		demo(); return
//		problemExample3(); return

		case class Config(
				                 input: String = "b a n a n a",
				                 inputFile: File = null,
				                 generateSimple: Seq[Int] = Seq(),
				                 generateMulti: Seq[Int] = Seq(),
				                 states: Int = 5,
				                 sequiturLattice: Boolean = false,
				                 maxArcRatio: Int = 5,
				                 allowThreshold: Double = 0.2,
				                 enforceThreshold: Double = 0.8,
				                 alpha: Double = 1.0,
				                 labelCoverage: Double = 0.5,
				                 tolerance: Double = 1e-5,
				                 maxEpochs: Int = 99,
		)
		val parser = new scopt.OptionParser[Config]("StructuredDocumentModel") {
			opt[String]("input").action( (x, c) =>
				c.copy(input = x)
			)
			opt[File]("input-file").action( (x, c) =>
				c.copy(inputFile = x)
			)
			opt[Seq[Int]]("generate-simple").action( (x, c) =>
				c.copy(generateSimple = x)
			)
			opt[Seq[Int]]("generate-multi").action( (x, c) =>
				c.copy(generateMulti = x)
			)
			opt[Int]("states").action( (x, c) =>
				c.copy(states = x)
			)
			opt[Unit]("sequitur-lattice").action( (_, c) =>
				c.copy(sequiturLattice = true)
			)
			opt[Int]("max-arc-ratio").action( (x, c) =>
				c.copy(maxArcRatio = x)
			)
			opt[Double]("allow-threshold").action( (x, c) =>
				c.copy(allowThreshold = x)
			)
			opt[Double]("enforce-threshold").action( (x, c) =>
				c.copy(enforceThreshold = x)
			)
			opt[Double]("alpha").action( (x, c) =>
				c.copy(alpha = x)
			)
			opt[Double]("label-coverage").action( (x, c) =>
				c.copy(labelCoverage = x)
			)
			opt[Double]("tolerance").action( (x, c) =>
				c.copy(tolerance = x)
			)
			opt[Int]("max-epochs").action( (x, c) =>
				c.copy(maxEpochs = x)
			)
		}
		val config = parser.parse(args, Config()) match {
			case Some(c) => c
			case None =>
				throw new Exception("Invalid arguments")
		}

		val labeledDoc = if (config.generateSimple.nonEmpty) {
			if (config.generateSimple.size != 4)
				throw new Exception(s"Wrong format for simpleItems parameters")
			val Seq(size, prefixLength, descLength, descVocabSize) = config.generateSimple
			val pcfg = DataGenerator.simpleItems(size, prefixLength, descLength, descVocabSize)
			DataGenerator.generateLabeledDoc(pcfg, List("DESCWORD"), config.labelCoverage, new Random(0))
		}
		else if (config.generateMulti.nonEmpty) {
			if (config.generateMulti.size != 1)
				throw new Exception(s"Wrong format for multiFieldItems parameters")
			val pcfg = DataGenerator.multiFieldItems(config.generateMulti.head,
			                                         (2, 10), (5, 20), (10, 20))
			DataGenerator.generateLabeledDoc(pcfg, List("FIELD1WORD", "FIELD2WORD", "FIELD3WORD"),
			                                 config.labelCoverage, new Random(0))
		}
		else if (config.inputFile != null)
			LabeledDoc(Source.fromFile(config.inputFile), config.labelCoverage)
		else
			LabeledDoc(config.input, config.labelCoverage)

		val doc =
			if (config.sequiturLattice) {
				val grammar = SequiturGrammar(labeledDoc.tokens).toStringGrammar

				DocumentLattice.fromStringGrammar(
				              grammar, config.allowThreshold, config.enforceThreshold, config.alpha,
											labeledDoc.labels)
			}
			else {
				DocumentLattice.fromTokens(
				              labeledDoc.tokens, config.maxArcRatio,
				              config.allowThreshold, config.enforceThreshold, config.alpha,
				              labeledDoc.labels)
			}
		println(doc.mkString())
//		return // just test DocumentLattice.fromGrammar

		val docs = List(doc)

		val initialModel = StructuredDocumentModel.randomInitial(
			config.states, DocumentLattice.buildVocab(docs))
		val (newModel, entropyLog) = initialModel.train(docs, FB, config.maxEpochs, config.tolerance)
		println(s"\nfinal model:\n$newModel")
		println(s"\nIterations: ${entropyLog.size}")
		println(s"Entropy log: $entropyLog")
		println()
//		println(newModel.viterbiChart(docs.head).toString)
		println(newModel.viterbiChart(docs.head).pathInfo())


//		ammonite.Main().run("tokens" → tokens, "text" → text, "doc" → doc)
	}
}
