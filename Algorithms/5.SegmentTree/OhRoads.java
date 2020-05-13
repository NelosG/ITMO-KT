import java.util.ArrayList;
import java.util.Random;
import java.util.Scanner;

import static java.lang.Integer.min;
import static java.lang.StrictMath.abs;

public class OhRoads {
    private static ArrayList<Node> nodes = new ArrayList();

    public static void main(String[] args) {
        Scanner in = new Scanner(System.in);
        int n = in.nextInt();
        int m = in.nextInt();
        int q = in.nextInt();
        for (int i = 0; i < n; i++) {
            nodes.add(new Node(i));
        }
        int i, j;
        for (int k = 0; k < m; k++) {
            i = in.nextInt() - 1;
            j = in.nextInt() - 1;
            insert(i, j);
        }
        String line;
        for (int k = 0; k < q; k++) {
            line = in.next();
            i = in.nextInt() - 1;
            j = in.nextInt() - 1;
            switch (line) {
                case "+":
                    insert(i, j);
                    break;
                case "-":
                    remove(i, j);
                    break;
                case "?":
                    System.out.println(query(i, j));
                    break;
            }
        }
    }

    private static int getSize(Node root) {
        if (root == null) return 0;
        return root.size;
    }

    private static void update(Node root) {
        if (root == null) return;
        root.size = 1 + getSize(root.leftChild) + getSize(root.rightChild);
    }

    private static TreapPair split(Node root, int pos) {
        if (root == null) return new TreapPair(null, null);
        int cur = getSize(root.leftChild);
        if (cur >= pos) {
            TreapPair leftSplit = split(root.leftChild, pos);
            root.leftChild = leftSplit.rightChild;
            if (leftSplit.rightChild != null) leftSplit.rightChild.parent = root;
            update(root);
            leftSplit.rightChild = root;
            return leftSplit;
        } else {
            TreapPair rightSplit = split(root.rightChild, pos - cur - 1);
            root.rightChild = rightSplit.leftChild;
            if (rightSplit.leftChild != null) rightSplit.leftChild.parent = root;
            update(root);
            rightSplit.leftChild = root;
            return rightSplit;
        }
    }

    private static Node merge(Node L, Node R) {
        if (L == null) return R;
        if (R == null) return L;
        if (L.prior > R.prior) {
            Node temp = merge(L.rightChild, R);
            L.rightChild = temp;
            temp.parent = L;
            update(L);
            return L;
        } else {
            Node temp = merge(L, R.leftChild);
            R.leftChild = temp;
            temp.parent = R;
            update(R);
            return R;
        }
    }

    private static void reverse(Node root) {
        if (root == null) return;
        Node temp = root.leftChild;
        root.leftChild = root.rightChild;
        root.rightChild = temp;
        reverse(root.leftChild);
        reverse(root.rightChild);
    }

    private static Node rootOf(Node k1) {
        Node k = k1;
        while (k.parent != null) k = k.parent;
        return k;
    }

    private static TreapPair getEnds(Node k) {
        var node1 = k;
        var node2 = k;
        while (node1.leftChild != null) node1 = node1.leftChild;
        while (node2.rightChild != null) node2 = node2.rightChild;
        return new TreapPair(node1, node2);
    }

    private static void insert(int i, int j) {
        Node s = nodes.get(i);
        Node t = nodes.get(j);
        Node sRoot = rootOf(s);
        Node tRoot = rootOf(t);
        if (sRoot == tRoot) {
            TreapPair ends = getEnds(sRoot);
            if (ends.leftChild == s && ends.rightChild == t || ends.leftChild == t && ends.rightChild == s)
                sRoot.reversed = true;
            return;
        }
        TreapPair sEnds = getEnds(sRoot);
        TreapPair tEnds = getEnds(tRoot);
        if (sEnds.rightChild == s && tEnds.leftChild == t)
            merge(sRoot, tRoot);
        else if (sEnds.leftChild == s && tEnds.rightChild == t)
            merge(tRoot, sRoot);
        else if (sEnds.leftChild == s && tEnds.leftChild == t) {
            reverse(sRoot);
            merge(sRoot, tRoot);
        } else if (sEnds.rightChild == s && tEnds.rightChild == t) {
            reverse(tRoot);
            merge(sRoot, tRoot);
        }
    }

    private static int findNumber(Node v1) {
        Node v = v1;
        int ans = getSize(v.leftChild);
        while (v.parent != null) {
            if (v == v.parent.rightChild) ans += getSize(v.parent.leftChild) + 1;
            v = v.parent;
        }
        return ans;
    }

    private static void remove(int i, int j) {
        Node s = nodes.get(i);
        Node t = nodes.get(j);
        Node sRoot = rootOf(s);
        TreapPair sEnds = getEnds(sRoot);
        if (sRoot.reversed) {
            sRoot.reversed = false;
            if (sEnds.leftChild == s && sEnds.rightChild == t
                    || sEnds.rightChild == s && sEnds.leftChild == t) return;
            int sNum = findNumber(s);
            int tNum = findNumber(t);
            if (sNum + 1 == tNum) {
                TreapPair kek = split(sRoot, tNum);
                kek.leftChild.parent = null;
                kek.rightChild.parent = null;
                Node iRoot = rootOf(nodes.get(i));
                Node jRoot = rootOf(nodes.get(j));
                reverse(iRoot);
                reverse(jRoot);
                merge(iRoot, jRoot);
            } else {
                TreapPair kek = split(sRoot, sNum);
                kek.leftChild.parent = null;
                kek.rightChild.parent = null;
                Node iRoot = rootOf(nodes.get(i));
                Node jRoot = rootOf(nodes.get(j));
                reverse(iRoot);
                reverse(jRoot);
                merge(jRoot, iRoot);
            }
            rootOf(nodes.get(i)).reversed = false;
            rootOf(nodes.get(j)).reversed = false;
            return;
        }
        int sNum = findNumber(s);
        int tNum = findNumber(t);
        if (sNum + 1 == tNum) {
            TreapPair kek = split(sRoot, tNum);
            kek.leftChild.parent = null;
            kek.rightChild.parent = null;
        } else {
            TreapPair kek = split(sRoot, sNum);
            kek.leftChild.parent = null;
            kek.rightChild.parent = null;
        }
    }

    private static int query(int i, int j) {
        if (i == j) return 0;
        Node s = nodes.get(i);
        Node t = nodes.get(j);
        Node sRoot = rootOf(s);
        Node tRoot = rootOf(t);
        if (sRoot != tRoot) return -1;
        int sNum = findNumber(s);
        int tNum = findNumber(t);
        if (!sRoot.reversed) return abs(tNum - sNum) - 1;
        var temp = sRoot;
        while (temp.rightChild != null) temp = temp.rightChild;
        int maximum = findNumber(temp);
        if (tNum > sNum)
            return min(maximum - tNum + sNum, tNum - sNum - 1);
        else
            return min(maximum - sNum + tNum, sNum - tNum - 1);
    }

    static class Node {
        int value;
        int prior = new Random().nextInt(131072);
        int size = 1;
        boolean reversed = false;
        Node leftChild = null;
        Node rightChild = null;
        Node parent = null;

        Node(int x) {
            this.value = x;
        }
    }

    static class TreapPair {
        Node leftChild, rightChild;

        TreapPair(Node leftChild, Node rightChild) {
            this.leftChild = leftChild;
            this.rightChild = rightChild;
        }
    }
}