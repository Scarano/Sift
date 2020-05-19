package structureextractor.rosettasuffixtree;

import java.util.ArrayList;
import java.util.List;

public class ReversedSuffixTree<A> extends SuffixTree<A> {

	int seqLength;

	public ReversedSuffixTree(List<A> seq, A endOfSeq) {
		super(reverse(seq), endOfSeq);
		seqLength = seq.size();
	}

	public static ReversedSuffixTree<String> ofString(List<String> seq) {
    return new ReversedSuffixTree<>(seq, "◆");
  }



  @Override public Node<A> lookup(int start, int end) {
    return super.lookup(this.seqLength - end, this.seqLength - start);
  }

	@Override public Node<A> lookup(List<A> query) {
		return super.lookup(reverse(query));
	}

	static <A> ArrayList<A> reverse(List<A> seq) {
		ArrayList<A> revSeq = new ArrayList<>(seq.size());
		for (int i = seq.size() - 1; i >= 0; i--)
			revSeq.add(seq.get(i));
		return revSeq;
	}
}
