package structureextractor.markovlattice

import structureextractor.util.Managed
import structureextractor.util.Managed._

import java.io.{File, FileOutputStream, PrintWriter}
import structureextractor.{DataGenerator, FrequencyCounter, FrequencyScorer, FrequencySegmenter, LabeledDoc}

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

	case class Config(input: String = "b a n a n a",
	                  inputFile: Option[File] = None,
	                  outputFile: Option[File] = None,
	                  pathOutputFile: Option[File] = None,
	                  generateSimple: Seq[Int] = Seq(),
	                  generateMulti: Seq[Int] = Seq(),
	                  states: Int = 5,
	                  rerunStates: Option[Int] = None,
	                  strategy: TrainingStrategy = FB,
	                  maxArcLength: Int = 10,
	                  arcPriorWeight: Double = 0.0,
	                  flatStates: Int = 0,
	                  flatStateBoost: Double = 0.0,
	                  frequencyCountLattice: Boolean = false,
	                  frequencyScoreWeight: Double = 0.5,
	                  frequencyNGramSize: Int = 3,
	                  frequencyCutoff: Int = 5,
	                  subsequenceLattice: Boolean = false,
	                  minArcFreq: Int = 5,
	                  maxArcRatio: Int = 5,
	                  sequiturLattice: Boolean = false,
	                  allowThreshold: Double = 0.2,
	                  enforceThreshold: Double = 0.8,
	                  alpha: Double = 1.0,
	                  labelCoverage: Double = 0.0,
	                  tolerance: Double = 1e-4,
	                  maxEpochs: Int = 99,
	                  truncate: Option[Int] = None,
	)

	def main(args: Array[String]): Unit = {
//		multiTest(args(0).toInt, List(DocumentLattice.examples(2))); return
//		demo(); return
//		problemExample3(); return

		val parser = new scopt.OptionParser[Config]("StructuredDocumentModel") {
			opt[String]("input").action( (x, c) =>
				c.copy(input = x)
			)
			opt[File]("input-file").action( (x, c) =>
				c.copy(inputFile = Some(x))
			)
			opt[File]("output-file").action( (x, c) =>
				c.copy(outputFile = Some(x))
			)
			opt[File]("path-output-file").action( (x, c) =>
				c.copy(pathOutputFile = Some(x))
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
			opt[Int]("rerun-states").action( (x, c) =>
				c.copy(rerunStates = Some(x))
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
			opt[Double]("arc-prior-weight").action( (x, c) =>
				c.copy(arcPriorWeight = x)
			)
			opt[Int]("flat-states").action( (x, c) =>
				c.copy(flatStates = x)
			)
			opt[Double]("flat-state-boost").action( (x, c) =>
				c.copy(flatStateBoost = x)
			)
			opt[Unit]("frequency-count-lattice").action( (_, c) =>
				c.copy(frequencyCountLattice = true)
			)
			opt[Double]("frequency-score-weight").action( (x, c) =>
				c.copy(frequencyScoreWeight = x)
			)
			opt[Int]("frequency-ngram-size").action( (x, c) =>
				c.copy(frequencyNGramSize = x)
			)
			opt[Int]("frequency-cutoff").action( (x, c) =>
				c.copy(frequencyCutoff = x)
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
		else {
			config.inputFile match {
				case Some(f) => LabeledDoc(Source.fromFile(f), config.labelCoverage, config.truncate)
				case _ => LabeledDoc(config.input, config.labelCoverage)
			}
		}

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
			else if (config.frequencyCountLattice) {
				val segmenter = FrequencySegmenter(labeledDoc.tokens, config.frequencyNGramSize,
					config.minArcFreq, config.frequencyCutoff)
				segmenter.makeDocumentLattice(labeledDoc.tokens, labeledDoc.labels)
			}
			else {
				DocumentLattice.fromTokens(labeledDoc.tokens, config.maxArcLength, labeledDoc.labels)
			}

		val logFile = config.outputFile.getOrElse(new File("/dev/null"))
		val logWriter = Managed(new PrintWriter(logFile))

		logWriter.use { log =>
			log.write(doc.mkString())
			log.write("\n\n")

			println(doc.mkString(limit=40))

			val docs = List(doc)

			val (model, charts) = runExperiment(config, config.states, docs, log)

			config.pathOutputFile.foreach { f =>
				new PrintWriter(f).use { writer =>
					for (chart <- charts)
						chart.printPath(writer)
				}
			}

			config.rerunStates.foreach { n =>
				val filteredDocs = charts.map { _.filterArcs() }
				runExperiment(config, n, filteredDocs, log)
			}
		}

//		ammonite.Main().run("tokens" → tokens, "text" → text, "doc" → doc)
	}

	def runExperiment[SYM: ClassTag](config: Config, states: Int, docs: Seq[DocumentLattice[SYM]],
	                                 log: PrintWriter)
	: (StructuredDocumentModel[SYM], Seq[ViterbiChart[SYM]]) = {

		val initialModel =
			StructuredDocumentModel.randomInitial(states, DocumentLattice.buildVocab(docs))
		val (model, lossLog) =
			initialModel.train(docs, config.strategy, config.maxEpochs, config.tolerance,
				                 config.arcPriorWeight, config.flatStates, config.flatStateBoost)
		val viterbiCharts = docs.map(model.viterbiChart(_))

		println(s"\nIterations: ${lossLog.size}")
		println(s"Loss log: " + lossLog.reverse.map(_.formatted("%.1f")).mkString(" "))

		log.write(s"\nfinal model:\n$model\n")
		log.write(s"Iterations: ${lossLog.size}\n")
		log.write(s"Loss log: " + lossLog.reverse.map(_.formatted("%.1f")).mkString(" ") + "\n")

		for (viterbiChart <- viterbiCharts.headOption) {
			println(viterbiChart.pathInfo(40, 80))
			println()

			log.write(viterbiChart.pathInfo() + "\n\n")

			viterbiChart.stateSummary(log, 10)
		}

//		println(model.viterbiChart(docs.head).toString)

		(model, viterbiCharts)
	}
}
