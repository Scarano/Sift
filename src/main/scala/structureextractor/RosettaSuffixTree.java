// This is copied from RosettaCode.org

package structureextractor;

import scala.Int;

import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

public class RosettaSuffixTree<A> {
    private static class Node<A> {
        List<A> subseq;
        List<Integer> ch = new ArrayList<>();  // list of child nodes

        public String toString() {
            if (null == subseq)
                return "ROOT";
            List<String> strings = subseq.stream().map(Object::toString)
                .collect(Collectors.toList());
            return String.join(" ", strings);
        }
    }

    private List<Node<A>> nodes = new ArrayList<>();

    public RosettaSuffixTree(List<A> seq) {
        nodes.add(new Node<>());
        for (int i = 0; i < seq.size(); ++i) {
            addSuffix(seq, i);
        }
    }

    private void addSuffix(List<A> suf, int from) {
        int size = suf.size() - from;
        int n = 0;
        int i = 0;
        while (i < size) {
            A b = suf.get(from + i);
            List<Integer> children = nodes.get(n).ch;
            int x2 = 0;
            int n2;
            while (true) {
                if (x2 == children.size()) {
                    // no matching child, remainder of suf becomes new node.
                    n2 = nodes.size();
                    Node<A> temp = new Node<>();
                    temp.subseq = suf.subList(from + i, suf.size());
                    nodes.add(temp);
                    children.add(n2);
                    return;
                }
                n2 = children.get(x2);
                if (nodes.get(n2).subseq.get(0) == b) break;
                x2++;
            }
            // find prefix of remaining suffix in common with child
            List<A> sub2 = nodes.get(n2).subseq;
            int j = 0;
            while (j < sub2.size()) {
                if (i + j >= size || !suf.get(from + i + j).equals(sub2.get(j))) {
                    // split n2
                    int n3 = n2;
                    // new node for the part in common
                    n2 = nodes.size();
                    Node<A> temp = new Node<>();
                    temp.subseq = sub2.subList(0, j);
                    temp.ch.add(n3);
                    nodes.add(temp);
                    nodes.get(n3).subseq = sub2.subList(j, sub2.size());  // old node loses the part in
                    // common
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
