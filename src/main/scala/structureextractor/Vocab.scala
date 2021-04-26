package structureextractor

import scala.collection.mutable
import scala.reflect.ClassTag
import scala.jdk.CollectionConverters._

class Vocab[S](val word_map: Array[S], val id_map: Map[S, Int],
               transform: S => S = identity[S] _)
{
	val id_map_fast = new java.util.HashMap(id_map.asJava)
	def size: Int = word_map.length

	def apply(token: S): Int = id_map_fast.get(transform(token))
	def apply(id: Int): S = word_map(id)
}

object Vocab {
	def build[S: ClassTag](corpus: Iterable[S], transform: S => S = identity[S] _): Vocab[S] = {
		val words = Array.newBuilder[S]
		val ids = mutable.Map.empty[S, Int]
		var next_id = 0
		for (token <- corpus) {
			val cls = transform(token)
			if (!(ids contains cls)) {
				words += cls
				ids += ((cls, next_id))
				next_id += 1
			}
		}
		new Vocab(words.result(), ids.toMap, transform)
	}

	def fromSymbols[S: ClassTag](symbols: Seq[S]): Vocab[S] = {
		new Vocab[S](symbols.toArray, symbols.zipWithIndex.toMap)
	}
}
