package structureextractor.markovlattice

import scala.collection.SeqMap


case class TrainingState[SYM] (
  hooks: Seq[TrainingState[SYM] => TrainingState[SYM]] = Nil,
  strategy: TrainingStrategy = FB,
  epoch: Int = 0,
  prevLosses: List[Double] = Nil,
  metrics: SeqMap[String, Double] = SeqMap.empty,
) {
  def metricsString: String =
    metrics.map({ case (k, v) => f"$k = $v%.5f" }).mkString(", ")
}
