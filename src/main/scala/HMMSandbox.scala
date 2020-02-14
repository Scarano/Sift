import scala.io.Source
import breeze.linalg._
import breeze.stats.distributions.RandBasis
import smile.sequence.HMM

import scala.collection.mutable.ListBuffer

class HMMSandbox {
}

object HMMSandbox {

	def probString(p: Double, prec: Int = 3) = {
		if (p == 0.0)
			"0"
		else if (p >= 1.0)
			"%.3f" format p
		else
			s"%.${(-Math.log(p) / Math.log(10.0)).toInt + 3}f" format p
	}

	def tokenize(s: String, preserve_whitespace: Boolean = false): Iterator[String] = {
		if (preserve_whitespace)
			"""\w+|\W""".r.findAllIn(s)
		else
			"""\w+|\S""".r.findAllIn(s)
	}

	def viterbi[O](hmm: HMM[O], seq: Array[Int], vocab: Vocab[O], debug: Boolean = false)
	: Double
	= {
		val states = hmm.predict(seq)
		val p_state = hmm.getInitialStateProbabilities
		val p_trans = hmm.getStateTransitionProbabilities
		val p_emit = hmm.getSymbolEmissionProbabilities

		var log_p_seq = 0.0
		var prev_state: Option[Int] = None
		for ((state, sym) <- states zip seq) {
			prev_state match {
				case None =>
					log_p_seq += Math.log(p_state(state))
					if (debug)
						println(f"START -> S$state (${p_state(state)}%.2f) ")
				case Some(prev) =>
					log_p_seq += Math.log(p_trans(prev)(state))
					if (debug)
						println(f"S$prev -> S$state (${p_trans(prev)(state)}%.2f)")
			}
			log_p_seq += Math.log(p_emit(state)(sym))
			if (debug) {
				val indent = " " * (20 + 3*state)
				println(f"${indent}S$state ~~> '${vocab(sym)}' (${p_emit(state)(sym)}%.2f)")
			}

			prev_state = Some(state)
		}
		log_p_seq
	}

	def randomUniform(size: Int, randBasis: RandBasis) = {
		val v = DenseVector.rand(size, randBasis.uniform)
		v /= sum(v)
		v.toArray
	}

	def main(args: Array[String]): Unit = {
		val nStates = args(0).toInt
		val input = if (args.length == 2) args(1) else "a b c a b d a b c a b d"

		val text = if (input(0) == '@') {
			val source = Source.fromFile(input.substring(1))
			try source.getLines.mkString finally source.close
		}
		else input

		val tokens = tokenize(text).toArray
		println(tokens.mkString("\u00b7"))

		val vocab: Vocab[String] = Vocab.build(tokens)
		val symbols = tokens.map(vocab.id_map)

		val randBasis = RandBasis.withSeed(123)
		val p_state = Array.fill(nStates) {1.0 / nStates}
		val p_trans = Array.fill(nStates) { randomUniform(nStates, randBasis) }
		val p_emit  = Array.fill(nStates) { randomUniform(vocab.size, randBasis) }

		val debugIterations = Set(1, 2, 4, 8, 16, 32)
		var hmm = new HMM[String](p_state, p_trans, p_emit)
		val logLikelihoods = new ListBuffer[Double]()
		logLikelihoods += viterbi(hmm, symbols, vocab, true)
		for (t <- 1 to debugIterations.max) {
			hmm = hmm.learn(Array(symbols), 1)
			if (debugIterations contains t) {
				println(s"\n________________\nTraining iteration $t:")
			}
			logLikelihoods += viterbi(hmm, symbols, vocab, debugIterations contains t)
		}

		println()
		for ((log_p, prev_log_p) <- logLikelihoods zip 0.0 :: logLikelihoods.toList) {
			println(f"$log_p (difference = ${log_p - prev_log_p})")
		}
	}
}
