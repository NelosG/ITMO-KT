import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.nio.charset.StandardCharsets;
import java.util.ArrayDeque;

public class GoblinsAndShamans {
    public static void main(String[] args) {
        StringBuilder sb = new StringBuilder();
        FastScanner sc = new FastScanner(System.in);
        int n = sc.nextInt();
        ArrayDeque deq1 = new ArrayDeque<Integer>();
        ArrayDeque deq2 = new ArrayDeque<Integer>();
        int size1 = 0;
        int size2 = 0;
        char tmp;
        for (int i = 0; i < n; i++) {
            tmp = sc.next();
            if (tmp == '+') {
                deq2.addLast(sc.nextInt());
                size2++;
            } else {
                if (tmp == '*') {
                    deq2.addFirst(sc.nextInt());
                    size2++;
                } else {
                    sb.append(deq1.pop() + "\n");
                    size1--;
                }
            }
            if (size1 < size2) {
                deq1.addLast(deq2.pop());
                size1++;
                size2--;
            }
        }
        System.out.print(sb);
    }

    static class FastScanner {


        private int pos, len;
        private char[] buffer;
        private boolean EOF = false;
        private InputStreamReader is;

        public FastScanner(InputStream in) {
            is = new InputStreamReader(in, StandardCharsets.UTF_8);

        }


        public char nextChar() {
            if (pos >= len) {
                readBuffer();
            }
            return buffer[pos++];
        }

        public boolean hasNextChar() {
            nextChar();
            pos--;
            return !EOF;
        }

        public int nextInt() {
            StringBuilder sb = new StringBuilder();
            char c;
            skipBlank();
            if (hasNextChar() && pos < len - 1) {
                c = nextChar();
                if (c != '\n') {
                    pos--;
                }
            }
            while (hasNextChar()) {
                c = nextChar();
                if (Character.isDigit(c) || c == '-' || c == '+') {
                    sb.append(c);
                } else {
                    break;
                }
            }

            return Integer.parseInt(sb.toString());

        }


        public Character next() {
            skipBlank();
            char c;
            c = nextChar();
            if (c == '\n' || c == ' ') {
                c = nextChar();
            } else {
                return c;
            }
            if (c == '\n' || c == ' ') {
                c = nextChar();
            } else {
                return c;
            }
            if (c == '\n' || c == ' ') {
                c = nextChar();
            }
            return c;
        }

        public void close() {
            try {
                is.close();
            } catch (IOException e) {

            }
        }


        private void readBuffer() {
            try {
                this.buffer = new char[100];
                this.len = is.read(this.buffer);
                while (len == 0) {
                    this.len = is.read(buffer);
                }
                if (this.len == -1) {
                    this.EOF = true;
                }
                this.pos = 0;
            } catch (IOException e) {

            }
        }


        private void skipBlank() {
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