package structureextractor.markovlattice

import structureextractor.util.Managed
import structureextractor.util.ManagedExtension._

import java.io.{File, PrintWriter}
import java.io.FileOutputStream
import structureextractor.{DataGenerator, FrequencySegmenter, LabeledDoc}

import scala.io.Source
import scala.reflect.ClassTag
import scala.util.Random
import structureextractor.Evaluation

object Experiment {
	def multiTest[SYM: ClassTag](numStates: Int, docs: List[DocumentLattice[SYM]]): Unit = {
		var entropies = List.empty[Double]
		val numTests = 10
		for (test <- 0 until numTests) {
			val initialModel = StructuredDocumentModel.randomInitial(
				numStates, 0, DocumentLattice.buildVocab(docs), test)
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
	                  tsvOutputFile: Option[File] = None,
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
	                  mergeData: Boolean = false,
	                  singletonCost: Double = 0.0,
	                  dataArcLevels: Int = 0,
	                  dataArcCost: Double = 0.0,
	                  subsequenceLattice: Boolean = false,
	                  minArcFreq: Int = 5,
	                  maxArcRatio: Int = 5,
	                  sequiturLattice: Boolean = false,
	                  allowThreshold: Double = 0.2,
	                  enforceThreshold: Double = 0.8,
	                  alpha: Double = 1.0,
	                  orderPrior: Option[Double] = None,
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
			opt[File]("tsv-file").action( (x, c) =>
				c.copy(tsvOutputFile = Some(x))
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
			opt[Unit]("merge-data").action( (_, c) =>
				c.copy(mergeData = true)
			)
			opt[Double]("singleton-cost").action( (x, c) =>
				c.copy(singletonCost = x)
			)
			opt[Int]("data-arc-levels").action( (x, c) =>
				c.copy(dataArcLevels = x)
			)
			opt[Double]("data-arc-cost").action( (x, c) =>
				c.copy(dataArcCost = x)
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
			opt[Double]("order-prior").action( (x, c) =>
				c.copy(orderPrior = Some(x))
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

		val testDoc = if (config.generateSimple.nonEmpty) {
			if (config.generateSimple.size != 4)
				throw new Exception(s"Wrong format for simpleItems parameters")
			val Seq(size, prefixLength, descLength, descVocabSize) = config.generateSimple
			val pcfg = DataGenerator.simpleItems(size, prefixLength, descLength, descVocabSize)
			DataGenerator.generateLabeledDoc(pcfg, List("DESCWORD"), 1.0, new Random(0))
		}
		else if (config.generateMulti.nonEmpty) {
			if (config.generateMulti.size != 1)
				throw new Exception(s"Wrong format for multiFieldItems parameters")
			val pcfg = DataGenerator.multiFieldItems(config.generateMulti.head,
			                                         (2, 10), (5, 20), (10, 20))
			DataGenerator.generateLabeledDoc(pcfg, List("FIELD1WORD", "FIELD2WORD", "FIELD3WORD"),
			                                 1.0, new Random(0))
		}
		else {
			config.inputFile match {
				case Some(f) => LabeledDoc(Source.fromFile(f), 1.0, None)
				case _ => LabeledDoc(config.input)
			}
		}

		val trainingDoc = LabeledDoc(
			testDoc.tokens, testDoc.labels, testDoc.labelNames, config.labelCoverage)

		val numLabels = trainingDoc.labelNames.size

		val doc =
			if (config.sequiturLattice) {
				val grammar = SequiturGrammar(trainingDoc.tokens).toStringGrammar

				DocumentLattice.fromStringGrammar(
				              grammar, config.allowThreshold, config.enforceThreshold, config.alpha,
											trainingDoc.labels)
			}
			else if (config.subsequenceLattice) {
				DocumentLattice.fromTokensUsingSubsequenceFinder(
				              trainingDoc.tokens, config.maxArcRatio,
				              config.minArcFreq, config.allowThreshold, config.alpha,
				              trainingDoc.labels)
			}
			else if (config.frequencyCountLattice) {
				val segmenter = FrequencySegmenter(trainingDoc.tokens, config.frequencyNGramSize,
					config.minArcFreq, config.frequencyCutoff)
				if (config.dataArcLevels == 0) {
					segmenter.makeDocumentLattice(trainingDoc.tokens, trainingDoc.labels,
				                                config.singletonCost, config.truncate)
				} else {
					segmenter.makeDocumentLatticeWithDataArcs(
					                              trainingDoc.tokens, trainingDoc.labels,
					                              config.dataArcLevels, config.dataArcCost, config.truncate)
				}
			}
			else {
				DocumentLattice.fromTokens(trainingDoc.tokens, config.maxArcLength, trainingDoc.labels)
			}

		val trainCallbacks = List({ model: StructuredDocumentModel[String] =>
			val chart = model.viterbiChart(doc)
			val predLabels = chartLabeling(testDoc, chart)
			val eval = Evaluation(testDoc, predLabels)

			println(
				List(eval.meanFscore, eval.meanPrec, eval.meanRec)
					.map(_.formatted("%.5f"))
					.mkString("\t"))
		})

		val logFile = config.outputFile.getOrElse(new File("/dev/null"))
		val logWriter = new PrintWriter(logFile)

		Managed(logWriter) { log =>
			log.write(doc.mkString())
			log.write("\n\n")

			println(doc.mkString(limit=40))

			val docs = List(doc)

			val (model, charts) = runExperiment(config, config.states, numLabels, docs, log, trainCallbacks)
			var finalCharts = charts

			config.pathOutputFile.foreach { f =>
				new PrintWriter(f).use { writer =>
					for (chart <- charts)
						chart.printPath(writer)
				}
			}

			config.rerunStates.foreach { n =>
				val filteredDocs = charts.map { _.filterArcs() }
				val (model2, charts2) = runExperiment(
					config, n, numLabels, filteredDocs, log, trainCallbacks)

				config.pathOutputFile.foreach { f =>
					Managed(new PrintWriter(new FileOutputStream(f, true))) { writer =>
						writer.println("\n\n\n==== rerun ====\n")
						for (chart <- charts2)
							chart.printPath(writer)
					}
				}

				finalCharts = charts2
			}

			config.tsvOutputFile.foreach { tsvFile =>
				Managed(new PrintWriter(tsvFile)) { tsvWriter => 
					for {
						chart <- finalCharts
					  record <- chart.filteredRecords
					} {
						val sanitized = record map { fieldValues => 
							fieldValues.map(_.sym).mkString(";").replace("\t", " ")
						}
						tsvWriter.print(sanitized.mkString("", "\t", "\n"))
					}
				}
			}

			finalCharts.foreach { chart =>
				val predLabels = chartLabeling(testDoc, chart)
				val evaluation = Evaluation(testDoc, predLabels)

				for (((prec, rec, fscore), labelName) <- evaluation.prfs zip testDoc.labelNames) {
					println(f"$labelName%20s: $fscore%.3f (pre=$prec%.3f rec=$rec%.3f)")
				}
				println(f"Mean: ${evaluation.meanFscore}%.3f (pre=${evaluation.meanPrec}%.3f rec=${evaluation.meanRec}%.3f)")
				println
			}
		}

//		ammonite.Main().run("tokens" → tokens, "text" → text, "doc" → doc)
	}

	def runExperiment(
		config: Config,
		states: Int,
		labelStates: Int,
		docs: Seq[DocumentLattice[String]],
		log: PrintWriter,
		hooks: Seq[StructuredDocumentModel[String] => Unit]
	): (StructuredDocumentModel[String], Seq[ViterbiChart[String]]) = {

		// TODO get rid of this hack after switching from String to a more general symbol class
		val wordTransform: String => String =
			if (config.mergeData)
				{ x => if (!x.startsWith("⸬")) "<<data>>" else x }
			else
				identity[String](_)

		val vocab = DocumentLattice.buildVocab(docs, wordTransform)

		val initialModel = StructuredDocumentModel.randomInitial(
			states, labelStates, vocab,
			arcPriorWeight = config.arcPriorWeight, orderPrior = config.orderPrior)
		val (model, lossLog) =
			initialModel.train(docs, config.strategy, config.maxEpochs, config.tolerance,
				                 config.flatStates, config.flatStateBoost, hooks)
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

	/**
	 * Given labeled reference document and list of "record" Vectors (in which each column
	 * corresponds to a state, and has zero or more spans of text that were observed for that
	 * state), determine which label each column (state) is likely to map to.
	 * 
	 * @return Vector [[mapping]] in which [[mapping(state)]] is either [[Some(labelNumber)]]
	 *   or [[None]] if no appropriate mapping was found for [[state]].
	 */
	def mapColumnsToLabels[SYM](doc: LabeledDoc, records: List[Vector[List[Span[SYM]]]])
	: Vector[Option[Int]] = {
		val numLabels = doc.labelNames.size
		val numColumns = records.head.size

		val mapping =
			for (c <- 0 until numColumns) yield {
				val numSpans = records.map( _(c).size ).sum
				val labels = for {
					record <- records
					span <- record(c)
					label <- doc.labels((span.start + span.end)/2)
				} yield label
				val labelCounts = labels.foldLeft(Vector.fill[Int](numLabels)(0)) { (counter, l) =>
					counter.updated(l, counter(l) + 1)
				}
				// Only map to a label that accounts for a majority of the spans in this column.
				labelCounts.zipWithIndex.filter(_._1 > numSpans / 2).headOption.map(_._2)
			}
		mapping.toVector
	}

	def chartLabeling[SYM](testDoc: LabeledDoc, chart: ViterbiChart[SYM]): Array[Option[Int]] = {
		val labelMap = mapColumnsToLabels(testDoc, chart.records)

		val predLabels = Array.fill[Option[Int]](testDoc.labels.size) { None }
		for {
			record <- chart.records
			(field, i) <- record.zipWithIndex
			label <- labelMap(i)
			span <- field
			t <- span.start until span.end
		} {
			predLabels(t) = Some(label)
		}
		predLabels
	}
}
