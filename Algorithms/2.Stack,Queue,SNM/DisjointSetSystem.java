import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.InputMismatchException;
import java.util.NoSuchElementException;

public class DisjointSetSystem {
    public static void main(String[] args) throws IOException {
        StringBuilder str = new StringBuilder();
        FastScanner sc = new FastScanner(System.in);
        int n = sc.nextInt();
        UG fus = new UG(n);
        while (sc.hasNextLine()) {
            String s = sc.next();
            if (s.equals("union")) {
                int x = sc.nextInt();
                int y = sc.nextInt();
                fus.union(fus.ro.get(x), fus.ro.get(y));
            }
            if (s.equals("get")) {
                int x = sc.nextInt();
                str.append(fus.get(fus.ro.get(x)) + "\n");
            }
        }
        sc.close();
        System.out.println(str.toString());
    }
}


class FastScanner {


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
        while (c == '\n' || c == '\r' || c == ' ') {
            c = nextChar();
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


class UG {

    ArrayList<Node> ro = new ArrayList<>();

    public UG(int n) {
        for (int i = 0; i <= n; i++) {
            ro.add(new Node(i));
        }
    }

    public String get(Node x) {
        Node setX = find(x);
        return setX.min + " " + setX.max + " " + setX.rank;
    }

    public void union(Node x, Node y) {
        Node setX = find(x);
        Node setY = find(y);
        if (setX.rank < setY.rank) {
            Node tmp = setX;
            setX = setY;
            setY = tmp;
        }
        if (setX.el == setY.el) {
            return;
        }
        setY.node = setX;
        if (setY.max > setX.max) {
            setX.max = setY.max;
        }
        if (setY.min < setX.min) {
            setX.min = setY.min;
        }
        setX.rank += setY.rank;
    }


    public Node find(Node x) {
        if (x.node == null) {
            return x;
        } else {
            x.node = find(x.node);
            return x.node;
        }

    }

}

class Node {
    Node node;
    int el;
    int rank = 1;
    int max;
    int min;

    public Node(int el) {
        this.el = el;
        this.max = el;
        this.min = el;
    }
}