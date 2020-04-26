import java.util.Random;
import java.util.Scanner;

public class SimpleBinSearchTree {
    public static Random random = new Random();
    public static Node Head;

    public static void main(String[] args) {
        Scanner scanner = new Scanner(System.in);
        String line;
        int key;
        Node tmp;
        boolean flag = true;
        while (scanner.hasNext()) {
            line = scanner.next();
            if (flag) {
                switch (line) {
                    case "insert":
                        key = scanner.nextInt();
                        Head = new Node(key, null, null);
                        flag = false;
                        break;
                    case "exists":
                        System.out.println("false");
                        break;
                    case "next":
                        System.out.println("none");
                        break;
                    case "prev":
                        System.out.println("none");
                        break;
                }
            } else {
                switch (line) {
                    case "insert":
                        key = scanner.nextInt();
                        Head = insert(Head, key);
                        break;
                    case "delete":
                        key = scanner.nextInt();
                        Head = delete(Head, key);
                        break;
                    case "exists":
                        key = scanner.nextInt();
                        if (exists(Head, key)) {
                            System.out.println("true");
                        } else {
                            System.out.println("false");
                        }
                        break;
                    case "next":
                        key = scanner.nextInt();
                        tmp = next(key);
                        if (tmp == null) {
                            System.out.println("none");
                        } else {
                            System.out.println(tmp.x);
                        }
                        break;
                    case "prev":
                        key = scanner.nextInt();
                        tmp = prev(key);
                        if (tmp == null) {
                            System.out.println("none");
                        } else {
                            System.out.println(tmp.x);
                        }
                        break;
                }
            }
        }
    }

    public static Node merge(Node L, Node R) {
        if (L == null) return R;
        if (R == null) return L;
        if (L.y > R.y) {
            L.right = merge(L.right, R);
            return L;
        } else {
            R.left = merge(L, R.left);
            return R;
        }
    }

    public static Pair split(Node t, int key) {
        if (t == null) {
            return new Pair(null, null);
        } else if (key >= t.x) {
            Pair pair = split(t.right, key);
            t.right = pair.left;
            return new Pair(t, pair.right);
        } else {
            Pair pair = split(t.left, key);
            t.left = pair.right;
            return new Pair(pair.left, t);
        }
    }

    public static Node insert(Node t, int key) {
        if (t == null) return new Node(key, null, null);
        Pair pair = split(t, key);
        Node tmp = new Node(key, null, null);
        tmp = merge(pair.left, tmp);
        return merge(tmp, pair.right);
    }

    public static Node delete(Node t, int key) {
        if (t == null) return null;
        Pair pair1 = split(t, key);
        Pair pair2 = split(pair1.left, key - 1);
        return merge(pair2.left, pair1.right);
    }

    public static Node search(Node t, int k) {
        if (t == null || k == t.x) {
            return t;
        }
        if (k < t.x) {
            return search(t.left, k);
        } else {
            return search(t.right, k);
        }
    }

    public static boolean exists(Node t, int x) {
        return search(t, x) != null;
    }

    public static Node next(int T) {
        Node current = Head;
        Node successor = null;
        while (current != null) {
            if (current.x > T) {
                successor = current;
                current = current.left;
            } else {
                current = current.right;
            }
        }
        return successor;
    }

    public static Node prev(int T) {
        Node current = Head;
        Node successor = null;
        while (current != null) {
            if (current.x < T) {
                successor = current;
                current = current.right;
            } else {
                current = current.left;
            }
        }
        return successor;
    }


    public static class Pair {
        Node left;
        Node right;

        Pair(Node left, Node right) {
            this.left = left;
            this.right = right;
        }
    }

    public static class Node {
        public int x;
        public int y;
        public Node left;
        public Node right;

        Node(int x, Node left, Node right) {
            this.x = x;
            this.y = random.nextInt();
            this.left = left;
            this.right = right;
        }
    }
}