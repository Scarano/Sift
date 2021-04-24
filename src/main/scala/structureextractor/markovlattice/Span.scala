package structureextractor.markovlattice

case class Span[SYM](start: Int, end: Int, sym: SYM, label: Option[Int])

object Span {
	def apply[SYM](start: Int, arc: AArc[SYM]): Span[SYM] = arc match {
		case Arc(sym, target, _) => Span(start, target, sym, None)
		case LabeledArc(sym, target, _, label) => Span(start, target, sym, Some(label))
	}
}
