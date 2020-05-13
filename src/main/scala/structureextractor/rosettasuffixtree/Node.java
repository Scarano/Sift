package structureextractor.rosettasuffixtree;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Node<A> {
    SuffixTree<A> tree;
    int start = -1;
    int end = -1;
    List<Integer> ch = new ArrayList<>();  // list of child nodes
    Integer count = null;

    public Node(SuffixTree<A> tree) {
        this.tree = tree;
    }

    public String toString() {
        if (start == -1)
            return "ROOT";
        List<String> strings = tree.seq.subList(start, end).stream().map(Object::toString)
            .collect(Collectors.toList());
        return "[" + getCount() + "] " +  String.join(" ", strings);
    }

    public int getCount() {
        if (count == null) {
            if (ch.isEmpty()) {
                count = 1;
            } else {
                Stream<Integer> childCounts = ch.stream().map(i -> tree.nodes.get(i).getCount());
                count = childCounts.reduce(0, Integer::sum);
            }
        }
        return count;
    }
}
