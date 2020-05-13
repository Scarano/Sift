// This is copied from RosettaCode.org

// While making changes and performance improvements, I realized this is the
// braindead O(n^2) suffix tree construction.
// TODO: Find or code the Ukkonen or Farach algorithm.

package structureextractor;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class RosettaSuffixTree<A> {
    class Node {
        int start = -1;
        int end = -1;
        List<Integer> ch = new ArrayList<>();  // list of child nodes
        Integer count = null;

        public String toString() {
            if (start == -1)
                return "ROOT";
            List<String> strings = seq.subList(start, end).stream().map(Object::toString)
                .collect(Collectors.toList());
            return "[" + getCount() + "] " +  String.join(" ", strings);
        }

        public int getCount() {
            if (count == null) {
                if (ch.isEmpty()) {
                    count = 1;
                } else {
                    Stream<Integer> childCounts = ch.stream().map(i -> nodes.get(i).getCount());
                    count = childCounts.reduce(0, Integer::sum);
                }
            }
            return count;
        }
    }

    private List<A> seq;
    private List<Node> nodes = new ArrayList<>();

    public RosettaSuffixTree(List<A> seq) {
        this.seq = seq;
        nodes.add(new Node());
        for (int i = 0; i < seq.size(); ++i) {
            addSuffix(i);
        }
    }

    private void addSuffix(int from) {
        int size = seq.size() - from;
        int n = 0;
        int i = 0;
        while (i < size) {
            A b = seq.get(from + i);
            List<Integer> children = nodes.get(n).ch;
            int x2 = 0;
            int n2;
            while (true) {
                if (x2 == children.size()) {
                    // no matching child, remainder of suf becomes new node.
                    n2 = nodes.size();
                    Node temp = new Node();
                    temp.start = from + i;
                    temp.end = seq.size();
                    nodes.add(temp);
                    children.add(n2);
                    return;
                }
                n2 = children.get(x2);
                if (seq.get(nodes.get(n2).start) == b) break;
                x2++;
            }
            // find prefix of remaining suffix in common with child
//            List<A> sub2 = nodes.get(n2).subseq;
            Node node2 = nodes.get(n2);
            int j = 0;
            while (j < node2.end - node2.start) {
                if (i + j >= size || !seq.get(from + i + j).equals(seq.get(node2.start + j))) {
                    // split n2
                    int n3 = n2;
                    // new node for the part in common
                    n2 = nodes.size();
                    Node temp = new Node();
                    temp.start = node2.start;
                    temp.end = node2.start + j;
                    temp.ch.add(n3);
                    nodes.add(temp);
                    // old node loses the part in common
                    nodes.get(n3).start = node2.start + j;
                    nodes.get(n3).end = node2.end;
                    nodes.get(n).ch.set(x2, n2);
                    break;  // continue down the tree
                }
                j++;
            }
            i += j;  // advance past part in common
            n = n2;  // continue down the tree
        }
    }

    public void visualize() {
        if (nodes.isEmpty()) {
            System.out.println("<empty>");
            return;
        }
        visualize_f(0, "");
    }

    private void visualize_f(int n, String pre) {
        List<Integer> children = nodes.get(n).ch;
        if (children.isEmpty()) {
            System.out.println("─ " + nodes.get(n));
            return;
        }
        System.out.println("┐ " + nodes.get(n));
        for (int i = 0; i < children.size() - 1; i++) {
            Integer c = children.get(i);
            System.out.print(pre + "├─");
            visualize_f(c, pre + "│ ");
        }
        System.out.print(pre + "└─");
        visualize_f(children.get(children.size() - 1), pre + "  ");
    }

    public static void main(String[] args) {
    	String s = args.length > 0 ? args[0] : "banana$";

    	try { Thread.sleep(200); } catch (InterruptedException e) {}
      System.out.println(" \n \n \n \n");

    	System.out.println(s);
    	ArrayList<Character> sChars = new ArrayList<>(s.length());
    	for (char c: s.toCharArray())
    	    sChars.add(c);
      new RosettaSuffixTree<>(sChars).visualize();
    }
}
