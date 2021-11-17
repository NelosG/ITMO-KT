import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.*;

public class ManagingDifficulties {
    public static void main(String[] agrs) {
        try {
            FastScanner sc = new FastScanner(System.in);
            int n = sc.nextInt();
            for (int d = 0; d < n; d++) {
                int k = sc.nextInt();
                List<Integer> list = new ArrayList<Integer>();
                for (int j = 0; j < k; j++) {
                    int tm = sc.nextInt();
                    list.add(tm);
                }
                int sum = 0;
                Map<Integer, Integer> map = new HashMap<Integer, Integer>();
                for (int j = k - 1; j >= 0; j--) {
                    for (int i = 0; i < j; i++) {
                        if (map.get(2 * list.get(j) - list.get(i)) != null) {
                            sum += map.get(2 * list.get(j) - list.get(i));
                        }
                    }
                    if (map.get(list.get(j)) != null) {
                        map.replace(list.get(j), map.get(list.get(j)) + 1);
                    } else {
                        map.put(list.get(j), 1);
                    }
                }
                System.out.println(sum);
            }
            sc.close();
        } catch (FileNotFoundException e) {
            System.out.println("Input file not found: " + e.getMessage());
        } catch (IOException e) {
            System.out.println("Input failed: " + e.getMessage());
        }
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
        is = new InputStreamReader(new FileInputStream(in), "UTF-8");

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

        return (char) buffer[pos++];
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