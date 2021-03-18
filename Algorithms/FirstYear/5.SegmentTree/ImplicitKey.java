import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.InputMismatchException;
import java.util.NoSuchElementException;
import java.util.Random;

public class ImplicitKey {

    private static long res = 0;

    private static long getNumberOfPeaks(Node T) {
        if (T == null) {
            return 0;
        }
        return T.size;
    }

    private static void update(Node T) {
        T.size = getNumberOfPeaks(T.right) + getNumberOfPeaks(T.left) + 1;
    }

    private static Node merge(Node A, Node B) {
        if (A == null)
            return B;
        if (B == null)
            return A;
        if (A.prior > B.prior) {
            A.right = merge(A.right, B);
            update(A);
            return A;
        } else {
            B.left = merge(A, B.left);
            update(B);
            return B;
        }
    }

    private static Pair split(Node A, long k) {
        if (A == null) {
            return new Pair(null, null);
        }
        if (getNumberOfPeaks(A.left) < k) {
            Pair p = split(A.right, k - 1 - getNumberOfPeaks(A.left));
            A.right = p.left;
            update(A);
            return new Pair(A, p.right);
        } else {
            Pair p = split(A.left, k);
            A.left = p.right;
            update(A);
            return new Pair(p.left, A);
        }
    }

    private static Node insert(Node A, Node node) {
        A = merge(A, node);
        return A;
    }

    private static Node delete(Node A, long x) {
        Pair p = split(A, x - 1);
        Pair p1 = split(p.right, 1);
        A = merge(p.left, p1.right);
        return A;
    }

    private static Node add(Node A, long k, long val) {
        Pair p = split(A, k);
        A = merge(p.left, new Node(val, new Random().nextInt()));
        A = merge(A, p.right);
        return A;
    }

    private static void print(Node T) {
        if (T.left != null) {
            print(T.left);
        }
        System.out.print(T.x + " ");
        if (T.right != null) {
            print(T.right);
        }
    }

    public static void main(String[] args) throws IOException {
        FastScanner sc = new FastScanner(System.in);
        int n = sc.nextInt();
        int m = sc.nextInt();
        Node Head = null;
        Random r = new Random();
        for (int i = 0; i < n; i++) {
            Head = insert(Head, new Node(sc.nextInt(), r.nextInt()));
        }
        String s = "";
        for (int i = 0; i < m; i++) {
            s = sc.next();
            if (s.equals("add")) {
                n++;
                Head = add(Head, sc.nextInt(), sc.nextInt());
            } else {
                n--;
                Head = delete(Head, sc.nextInt());
            }
        }
        System.out.println(n);
        if (n != 0) print(Head);
    }

    private static class Node {
        long size;
        long x, prior;
        Node left, right;

        Node(long x, long prior) {
            this.x = x;
            this.prior = prior;
            this.size = 1;
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