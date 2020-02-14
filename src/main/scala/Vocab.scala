import scala.collection.mutable
import scala.reflect.ClassTag

class Vocab[SYM](val word_map: Array[SYM], val id_map: Map[SYM, Int]) {
	def size: Int = word_map.length

	def apply(token: SYM): Int = id_map(token)
	def apply(id: Int): SYM = word_map(id)
}

object Vocab {
	def build[SYM: ClassTag](corpus: Seq[SYM]): Vocab[SYM] = {
		val words = Array.newBuilder[SYM]
		val ids = mutable.Map.empty[SYM, Int]
		var next_id = 0
		for (token <- corpus) {
			if (!(ids contains token)) {
				words += token
				ids += ((token, next_id))
				next_id += 1
			}
		}
		new Vocab(words.result(), ids.toMap)
	}

	def fromSymbols[SYM: ClassTag](symbols: Seq[SYM]): Vocab[SYM] = {
		new Vocab[SYM](symbols.toArray, symbols.zipWithIndex.toMap)
	}
}
