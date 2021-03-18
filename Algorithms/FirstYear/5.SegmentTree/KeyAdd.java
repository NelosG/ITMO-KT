import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.InputMismatchException;
import java.util.NoSuchElementException;
import java.util.Random;

public class KeyAdd {
    public static FastScanner scanner = new FastScanner(System.in);
    public static Random random = new Random();
    public static Node Head;
    public static ArrayList<Long> Result = new ArrayList<>();
    public static long m;

    public static void main(String[] args) throws IOException {
        long n = scanner.nextInt();
        m = scanner.nextInt();
        create(m + n);
        for (long i = 0; i < n; i++) {
            Head = insert(Head, i + 1, scanner.nextInt() - 1);
        }
        Res(Head);
        int tmp = Result.size();
        for (int i = tmp - 1; i > -1; i--) {
            if (Result.get(i) == 0) Result.remove(i);
            else break;
        }
        System.out.println(Result.size());
        for (int i = 0; i < Result.size(); i++) {
            System.out.print(Result.get(i) + " ");
        }
    }

    public static void Res(Node T) {
        if (T != null) {
            Res(T.left);
            Result.add(T.x);
            Res(T.right);
        }
    }

    public static long getSize(Node T) {
        if (T == null) return 0;
        return T.size;
    }

    public static void updateSize(Node T) {
        if (T == null) return;
        T.size = 1 + getSize(T.left) + getSize(T.right);
        if (T == null) T.zero = 0;
        else T.zero = (T.left == null ? 0 : T.left.zero) + (T.right == null ? 0 : T.right.zero) + (T.x != 0 ? 0 : 1);
    }

    public static void create(long n) {
        for (long i = 0; i < n; i++) Head = merge(Head, new Node(0, null, null));
    }

    public static Node removeZero(Node T) {
        if (T.left != null && T.left.zero > 0) T.left = removeZero(T.left);
        else if (T.x == 0) T = merge(T.left, T.right);
        else T.right = removeZero(T.right);
        updateSize(T);
        return T;
    }

    public static Node merge(Node L, Node R) {
        if (L == null) {
            updateSize(R);
            return R;
        }
        if (R == null) {
            updateSize(L);
            return L;
        }
        if (L.y > R.y) {
            L.right = merge(L.right, R);
            updateSize(L);
            return L;
        } else {
            R.left = merge(L, R.left);
            updateSize(R);
            return R;
        }
    }


    public static Pair split(Node T, long key) {
        if (T == null) return new Pair(null, null);
        if (getSize(T.left) >= key) {
            Pair pair = split(T.left, key);
            T.left = pair.right;
            updateSize(T);
            return new Pair(pair.left, T);
        } else {
            Pair pair = split(T.right, key - getSize(T.left) - 1);
            T.right = pair.left;
            updateSize(T);
            return new Pair(T, pair.right);
        }
    }


    public static Node insert(Node T, long key, long pos) {
        Pair pair = split(T, pos);
        pair.right = removeZero(pair.right);
        return merge(merge(pair.left, new Node(key, null, null)), pair.right);
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
        public long x, y;
        public long size = 1;
        public Node left;
        public Node right;
        public long zero;

        Node(long x, Node left, Node right) {
            this.x = x;
            this.y = random.nextInt();
            this.left = left;
            this.right = right;
            if (x == 0) {
                this.zero = 1;
            } else {
                this.zero = 0;
            }
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