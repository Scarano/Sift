package structureextractor

import structureextractor.preprocessing.LabeledDoc

import scala.collection.SeqMap

case class Evaluation(
  prfs: Seq[(Double, Double, Double)],
  meanPrec: Double,
  meanRec: Double,
  meanFscore: Double
) {
	def asMap: SeqMap[String, Double] =
		SeqMap(
			"meanFscore" -> meanFscore,
			"meanPrec" -> meanPrec,
			"meanRec" -> meanRec,
		)
}

object Evaluation {
  def apply(testDoc: LabeledDoc, predLabels: Seq[Option[Int]]): Evaluation = {
    val prfs =
      for (testLabel <- testDoc.labelNames.indices)
        yield precisionRecallFscore(testDoc.labels, predLabels, testLabel)
    val meanPrec = prfs.map(_._1).sum / prfs.size
    val meanRec = prfs.map(_._2).sum / prfs.size
    val meanFscore = prfs.map(_._3).sum / prfs.size

    new Evaluation(prfs, meanPrec, meanRec, meanFscore)
  }

	def precisionRecallFscore(
		refLabels: Seq[Option[Int]],
		predLabels: Seq[Option[Int]],
		testLabel: Int)
	: (Double, Double, Double) = {
		var tp = 0.0
		var fp = 0.0
		var fn = 0.0
		refLabels zip predLabels foreach {
			case (Some(y), Some(y_pred)) if y == testLabel && y_pred == testLabel =>
				tp += 1.0
			case (Some(y), _) if y == testLabel =>
				fn += 1.0
			case (_, Some(y_pred)) if y_pred == testLabel =>
				fp += 1.0
			case _ => ()
		}
		val prec =
			if (tp + fp == 0.0) 1.0
			else tp / (tp + fp)
		val rec =
			if (tp + fn == 0.0) 1.0
			else tp / (tp + fn)
		val fscore =
			if (prec * rec == 0.0) 0.0
			else 2.0 / (1.0/prec + 1.0/rec)
		(prec, rec, fscore)
	}
}
