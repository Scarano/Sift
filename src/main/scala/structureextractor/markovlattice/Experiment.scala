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
				                 strategy: TrainingStrategy = FB,
				                 maxArcLength: Int = 10,
				                 arcLengthPenalty: Double = 0.0,
				                 subsequenceLattice: Boolean = false,
				                 minArcFreq: Int = 5,
				                 maxArcRatio: Int = 5,
				                 sequiturLattice: Boolean = false,
				                 allowThreshold: Double = 0.2,
				                 enforceThreshold: Double = 0.8,
				                 alpha: Double = 1.0,
				                 labelCoverage: Double = 0.0,
				                 tolerance: Double = 1e-5,
				                 maxEpochs: Int = 99,
				                 truncate: Option[Int] = None,
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
			opt[String]("strategy").action( (x, c) =>
				if (x == "fb")
					c.copy(strategy = FB)
				else if (x == "viterbi")
					c.copy(strategy = Viterbi)
				else if (x == "hybrid")
					c.copy(strategy = FBThenViterbi)
				else
					throw new Exception(s"Invalid strategy '$x'")
			)
			opt[Int]("max-arc-length").action( (x, c) =>
				c.copy(maxArcLength = x)
			)
			opt[Double]("arc-length-penalty").action( (x, c) =>
				c.copy(arcLengthPenalty = x)
			)
			opt[Unit]("subsequence-lattice").action( (_, c) =>
				c.copy(subsequenceLattice = true)
			)
			opt[Int]("min-arc-freq").action( (x, c) =>
				c.copy(minArcFreq = x)
			)
			opt[Int]("max-arc-ratio").action( (x, c) =>
				c.copy(maxArcRatio = x)
			)
			opt[Unit]("sequitur-lattice").action( (_, c) =>
				c.copy(sequiturLattice = true)
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
			opt[Int]("truncate").action( (x, c) =>
				c.copy(truncate = Some(x))
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
			LabeledDoc(Source.fromFile(config.inputFile), config.labelCoverage, config.truncate)
		else
			LabeledDoc(config.input, config.labelCoverage)

		val doc =
			if (config.sequiturLattice) {
				val grammar = SequiturGrammar(labeledDoc.tokens).toStringGrammar

				DocumentLattice.fromStringGrammar(
				              grammar, config.allowThreshold, config.enforceThreshold, config.alpha,
											labeledDoc.labels)
			}
			else if (config.subsequenceLattice) {
				DocumentLattice.fromTokensUsingSubsequenceFinder(
				              labeledDoc.tokens, config.maxArcRatio,
				              config.minArcFreq, config.allowThreshold, config.alpha,
				              labeledDoc.labels)
			}
			else {
				DocumentLattice.fromTokens(labeledDoc.tokens, config.maxArcLength, labeledDoc.labels)
			}
//		println(doc.mkString())
		println(doc.mkString(limit=400))

		val docs = List(doc)

		val initialModel = StructuredDocumentModel.randomInitial(
			config.states, DocumentLattice.buildVocab(docs))
		val (model, lossLog) =
			initialModel.train(docs, config.strategy, config.maxEpochs, config.tolerance,
				                 config.arcLengthPenalty)
		println(s"\nfinal model:\n$model")
		println(s"\nIterations: ${lossLog.size}")
		println(s"Loss log: " + lossLog.reverse.map(_.formatted("%.1f")).mkString(" "))
		println()
//		println(model.viterbiChart(docs.head).toString)
		println(model.viterbiChart(docs.head).pathInfo(20))
//		println(model.viterbiChart(docs.head).pathInfo())

		val filteredDocs = docs.map { doc => model.viterbiChart(doc).filterArcs() }

		val fInitialModel = StructuredDocumentModel.randomInitial(
			config.states, DocumentLattice.buildVocab(filteredDocs))
		val (fModel, fLossLog) =
			fInitialModel.train(filteredDocs, config.strategy, config.maxEpochs,
			                                   config.tolerance, 0.0)
		println(s"\nFiltered model:\n$fModel")
		println(s"\nIterations: ${fLossLog.size}")
		println(s"Loss log: " + fLossLog.reverse.map(_.formatted("%.1f")).mkString(" "))
		println()
		println(fModel.viterbiChart(filteredDocs.head).pathInfo(20))
//		println(fModel.viterbiChart(filteredDocs.head).pathInfo())

//		ammonite.Main().run("tokens" → tokens, "text" → text, "doc" → doc)
	}
}
