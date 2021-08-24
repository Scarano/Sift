package structureextractor.markovlattice

import scala.collection.SeqMap


case class TrainingState[SYM] (
  docs: Seq[DocumentLattice[SYM]] = Nil,
  model: StructuredDocumentModel[SYM],
  strategy: TrainingStrategy = FB,
  temperature: Double = -100,
  epoch: Int = 0,
  prevLosses: List[Double] = Nil,
  metrics: SeqMap[String, Double] = SeqMap.empty,
  epochStart: Long = System.currentTimeMillis()
) {

  lazy val viterbiCharts = docs.map(doc => model.viterbiChart(doc))

  def metricsString: String =
    metrics.map({ case (k, v) => f"$k = $v%.5f" }).mkString(", ")
}