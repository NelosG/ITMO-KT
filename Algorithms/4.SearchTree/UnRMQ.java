import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class UnRMQ {
    static int one;
    static long[] ar;

    public static void main(String[] args) throws IOException {
        File in = new File("rmq.in");
        FastScanner sc = new FastScanner(in);
        FileWriter wr = new FileWriter("rmq.out");
        StringBuilder sb = new StringBuilder();
        Comparator comp = new MyComparator();
        int n = sc.nextInt();
        int m = sc.nextInt();
        one = 1;
        while (one < n) {
            one *= 2;
        }
        ar = new long[2 * one - 1];
        ArrayList<Zap> zap = new ArrayList<>();
        for (int i = 0; i < m; i++) {
            zap.add(new Zap());
        }
        for (int i = 0; i < one * 2 - 1; i++) {
            ar[i] = Long.MAX_VALUE;
        }
        for (int i = 0; i < m; i++) {
            zap.get(i).i = sc.nextInt() - 1;
            zap.get(i).j = sc.nextInt();
            zap.get(i).q = sc.nextInt();
        }
        sc.close();
        Collections.sort(zap, comp);
        for (int i = 0; i < m; i++) {
            update(0, 0, one, zap.get(i).i, zap.get(i).j, zap.get(i).q);
        }

        build(0);
        for (int i = one - 2; i >= 0; i--) {
            ar[i] = minn(ar[2 * i + 1], ar[2 * i + 2]);
        }
        boolean flag = true;
        for (int i = 0; i < m; i++) {
            if (min(0, 0, one, zap.get(i).i, zap.get(i).j) != zap.get(i).q) {
                flag = false;
                break;
            }
        }
        if (!flag) {
            sb.append("inconsistent\n");
        } else {
            sb.append("consistent\n");
            for (int i = one - 1; i < one - 1 + n; i++) {
                if (ar[i] >= Integer.MAX_VALUE) {
                    sb.append(Integer.MAX_VALUE + " ");
                } else {
                    sb.append(ar[i] + " ");
                }
            }
        }
        wr.write(sb.toString());
        wr.close();
    }

    static long minn(long a, long b) {
        if (a < b) return a;
        return b;
    }

    static long min(int v, int l, int r, int a, int b) {
        if (l >= b || a >= r) return Long.MAX_VALUE;
        if (a <= l && r <= b) return ar[v];
        return minn(min(2 * v + 1, l, (l + r) / 2, a, b), min(2 * v + 2, (l + r) / 2, r, a, b));
    }

    public static void update(int v, int l, int r, int a, int b, int x) {
        if (b <= l || r <= a) {
            return;
        }
        if (l >= a && r <= b) {
            ar[v] = x;
            return;
        }
        int m = (l + r) / 2;
        update(2 * v + 1, l, m, a, b, x);
        update(2 * v + 2, m, r, a, b, x);
    }

    public static void build(int v) {
        if (v >= one - 1) {
            return;
        }
        if (ar[v] != Long.MAX_VALUE) {
            check(2 * v + 1);
            check(2 * v + 2);
        }
        build(2 * v + 1);
        build(2 * v + 2);
    }

    public static void check(int v) {
        if (ar[v] == Long.MAX_VALUE) {
            ar[v] = ar[(v - 1) / 2];
        }
        if (ar[v] < ar[(v - 1) / 2]) {
            ar[v] = ar[(v - 1) / 2];
        }
    }

    public static class Zap {
        int i = 0;
        int j = 0;
        int q = 0;
        Zap() {

        }
    }

    public static class MyComparator implements Comparator<Zap> {
        public int compare(Zap a, Zap b) {
            return Integer.compare(a.q, b.q);
        }
    }

    static class FastScanner {


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
            //System.out.print("| " + len + " | ");		
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