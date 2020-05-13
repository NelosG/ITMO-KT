import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.InputMismatchException;
import java.util.NoSuchElementException;
import java.util.Random;

public class SumAgain {

    private static long res = 0;

    private static long getNumberOfPeaks(Node T) {
        if (T == null) {
            return 0;
        }
        return T.sum;
    }

    private static void update(Node T) {
        T.sum = getNumberOfPeaks(T.right) + getNumberOfPeaks(T.left) + T.x;
    }

    private static Node merge(Node A, Node B) {
        if (A == null)
            return B;
        if (B == null)
            return A;
        if (A.y > B.y) {
            A.right = merge(A.right, B);
            update(A);
            return A;
        } else {
            B.left = merge(A, B.left);
            update(B);
            return B;
        }
    }

    private static Pair split(Node A, long val) {
        if (A == null) {
            return new Pair(null, null);
        }
        if (A.x < val) {
            Pair p = split(A.right, val);
            A.right = p.left;
            update(A);
            return new Pair(A, p.right);
        } else {
            Pair p = split(A.left, val);
            A.left = p.right;
            update(A);
            return new Pair(p.left, A);
        }
    }

    private static Node insert(Node A, Node node) {
        if (exists(A, node.x)) return A;
        Pair p = split(A, node.x);
        p.left = merge(p.left, node);
        A = merge(p.left, p.right);
        return A;
    }

    private static Node delete(Node A, long x) {
        Pair p = split(A, x);
        Pair p1 = split(p.right, x + 1);
        A = merge(p.left, p1.right);
        return A;
    }

    private static boolean exists(Node A, long x) {
        if (A == null)
            return false;
        if (A.x == x)
            return true;
        if (A.x > x) {
            return exists(A.left, x);
        } else {
            return exists(A.right, x);
        }
    }

    public static Node sum(Node node, long left, long right) {
        Pair one = split(node, right);
        Pair two = split(one.left, left);
        res = getNumberOfPeaks(two.right);
        return merge(two.left, merge(two.right, one.right));
    }

    public static void write(Node A) {
        if (A != null) {
            write(A.left);
            System.out.print(A.x + "___");
            write(A.right);
        }
    }

    public static void main(String[] args) throws IOException {
        FastScanner sc = new FastScanner(System.in);
        int n = sc.nextInt();
        Node Head = null;
        String s = "";
        String prev = "";
        for (int i = 0; i < n; i++) {
            prev = s;
            s = sc.next();
            if (s.equals("+")) {
                long temp = sc.nextInt();
                if (prev.equals("?")) {
                    temp = (temp + res) % 1_000_000_000;
                }
                if (!exists(Head, temp)) {
                    Head = insert(Head, new Node(temp, new Random().nextInt()));
                }
            } else {
                Head = sum(Head, sc.nextInt(), sc.nextInt() + 1);
                System.out.println(res);
            }
        }
    }

    private static class Node {
        long sum;
        long x, y;
        Node left, right;

        Node(long x, long y) {
            this.x = x;
            this.y = y;
            this.sum = x;
        }
    }

    private static class Pair {
        Node left, right;

        Pair(Node left, Node right) {
            this.left = left;
            this.right = right;
        }
    }

    public static class FastScanner implements AutoCloseable {


        private int pos, len;
        private char[] buffer;
        private boolean EOF = false;
        private InputStreamReader is;

        public FastScanner(InputStream in) {
            is = new InputStreamReader(in, StandardCharsets.UTF_8);

        }


        public FastScanner(String in) {
            in += " ";
            is = new InputStreamReader(new ByteArrayInputStream(in.getBytes(StandardCharsets.UTF_8)), StandardCharsets.UTF_8);

        }

        public FastScanner(File in) throws IOException {
            is = new InputStreamReader(new FileInputStream(in), StandardCharsets.UTF_8);

        }

        public ArrayList<String> nextArray() throws IOException {
            ArrayList<String> a = new ArrayList<>();
            String s = nextLine().toLowerCase();

            StringBuilder tmp = new StringBuilder();

            for (int i = 0; i < s.length(); i++) {
                char c = s.charAt(i);
                while (Character.isLetter(c) || Character.getType(c) == Character.DASH_PUNCTUATION || c == '\'') {
                    tmp.append(c);
                    i++;
                    if (i < s.length()) {
                        c = s.charAt(i);
                    } else {
                        break;
                    }
                }

                if (!tmp.toString().isEmpty()) {
                    a.add(tmp.toString());
                    tmp.setLength(0);
                }
            }

            return a;
        }

        public char nextChar() throws IOException {
            if (pos >= len) {
                readBuffer();
            }

            return buffer[pos++];
        }

        public boolean hasNextChar() throws IOException {
            nextChar();
            pos--;
            return !EOF;
        }

        public boolean hasNextLine() throws IOException {
            char c;
            c = nextChar();
            if (c == '\n') {

            }
            pos--;
            return !EOF;
        }

        public String nextLine() throws IOException {
            StringBuilder sb = new StringBuilder();
            char c;
            while (hasNextChar()) {
                c = nextChar();
                if (c == '\n') {
                    break;
                }
                if (c != '\r') {
                    sb.append(c);
                }
            }
            return sb.toString();
        }


        public boolean hasNextInt() throws IOException {
            skipBlank();
            char c;
            boolean res = true;
            if (!hasNextChar()) {
                res = false;
            } else {
                if (pos < len - 1) {
                    c = nextChar();
                    if (c != '\n' && c != '\r') {
                        if (!Character.isDigit(c) && c != '-' && c != '+') {
                            res = false;
                        }
                        pos--;
                    }
                }


            }
            return res;
        }

        public int nextInt() throws IOException {
            StringBuilder sb = new StringBuilder();
            char c;
            skipBlank();
            if (hasNextChar() && pos < len - 1) {
                c = nextChar();
                if (c != '\n') {
                    pos--;
                }
            } else {
                if (!hasNextChar()) {
                    throw new InputMismatchException();
                }
            }

            while (hasNextChar()) {
                c = nextChar();
                if (Character.isDigit(c) || c == '-' || c == '+') {
                    sb.append(c);
                } else {
                    if (!Character.isWhitespace(c)) {
                        throw new InputMismatchException();
                    }
                    break;
                }
            }

            try {
                if (sb.length() != 0) {
                    return Integer.parseInt(sb.toString());
                } else {
                    throw new NumberFormatException();
                }
            } catch (NumberFormatException e) {
                throw new InputMismatchException();
            }

        }

        public boolean hasNext() throws IOException {
            return hasNextLine();

        }

        public String next() throws IOException {
            skipBlank();
            StringBuilder sb = new StringBuilder();
            char c;
            while (hasNextChar()) {
                c = nextChar();
                if (c == '\n' && sb.length() != 0) {
                    break;
                }
                if (c != ' ') {
                    sb.append(c);
                } else {
                    break;
                }
            }
            if (sb.length() != 0) {
                return sb.toString();
            } else {
                if (EOF) {
                    throw new NoSuchElementException();
                }
            }
            return "";

        }

        public void close() throws IOException {
            is.close();
        }


        private void readBuffer() throws IOException {
            this.buffer = new char[300];
            this.len = is.read(this.buffer);
            while (len == 0) {
                this.len = is.read(buffer);
            }
            if (this.len == -1) {
                this.EOF = true;
            }
            this.pos = 0;
        }


        private void skipBlank() throws IOException {
            while (true) {
                if (hasNextChar()) {
                    char c = nextChar();
                    if (!Character.isWhitespace(c)) {
                        pos--;
                        break;
                    }

                } else {
                    break;
                }
            }

        }


    }

}