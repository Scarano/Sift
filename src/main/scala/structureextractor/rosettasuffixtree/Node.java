package structureextractor.rosettasuffixtree;

import java.util.*;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class Node<A> {
    SuffixTree<A> tree;
    int start = -1;
    int end = -1;
    List<Integer> children = new ArrayList<>();  // list of child nodes
    int depth = 0;
    Integer count = null;
    List<Integer> occurrences = null;

    public Node(SuffixTree<A> tree) {
        this.tree = tree;
    }

    public SuffixTree<A> getTree() { return tree; }
    public int getStart() { return start; }
    public int getEnd() { return end; }
    public int getDepth() { return depth; }
    public Iterator<Node<A>> getChildren() {
        final Iterator<Integer> childIter = children.iterator();
        return new Iterator<>() {
            @Override public boolean hasNext() {
                return childIter.hasNext();
            }

            @Override public Node<A> next() {
                return tree.nodes.get(childIter.next());
            }
        };
    }

    public void populateDepth(int depth) {
        this.depth = depth + getLength();
        for (int c: children)
            tree.nodes.get(c).populateDepth(this.depth);
    }

    public int getLength() {
        return end - start;
    }

    public String toString() {
        if (start == -1)
            return "ROOT";

        List<String> strings;

        if (start - end <= 30) {
            strings = tree.seq.subList(start, end).stream().map(Object::toString)
                        .collect(Collectors.toList());
        }
        else {
            strings = tree.seq.subList(start, start + 15).stream().map(Object::toString)
                        .collect(Collectors.toList());
            strings.add("...");
            strings.addAll(tree.seq.subList(end - 15, end).stream().map(Object::toString)
                             .collect(Collectors.toList()));
        }
        return "[" + getCount() + "] " +  String.join(" ", strings);
    }

    public int getCount() {
        if (count == null) {
            if (children.isEmpty()) {
                count = 1;
            } else {
                Stream<Integer> childCounts = children.stream().map(i -> tree.nodes.get(i).getCount());
                count = childCounts.reduce(0, Integer::sum);
            }
        }
        return count;
    }

    public List<Integer> getOccurrences() {
        if (occurrences == null) {
            if (children.isEmpty()) {
                occurrences = Arrays.asList(tree.seq.size() - depth);
            }
            else {
                occurrences = new ArrayList<>();
                for (int c: children) {
                    Node<A> child = tree.nodes.get(c);
                    occurrences.addAll(child.getOccurrences());
                }
            }
        }
        return occurrences;
    }

    public Node<A> lookup(int qStart, int qEnd) {
        int i = 0;
        while (true) {
            if (qStart + i == qEnd)
                return this;
            if (i == getLength())
                break;
            if (!tree.seq.get(start + i).equals(tree.seq.get(qStart + i)))
                return null;
            i++;
        }
        for (int c: children) {
            Node<A> result = tree.nodes.get(c).lookup(qStart + i, qEnd);
            if (result != null)
                return result;
        }
        return null;
    }
}
