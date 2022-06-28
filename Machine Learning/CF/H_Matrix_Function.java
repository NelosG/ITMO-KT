import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.stream.IntStream;
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.InputMismatchException;
import java.util.NoSuchElementException;

public class H_Matrix_Function {
    public static ArrayList<Double> cloneA(ArrayList<Double> a) {
        ArrayList<Double> copy = new ArrayList<>();
        IntStream.range(0, a.size()).forEach(i -> copy.add(a.get(i)));
        return copy;
    }

    public static ArrayList<ArrayList<Double>> cloneAA(ArrayList<ArrayList<Double>> a) {
        ArrayList<ArrayList<Double>> copy = new ArrayList<>();
        IntStream.range(0, a.size()).forEach(i -> copy.add(cloneA(a.get(i))));
        return copy;
    }
    public static class Pair<F, S> {
        F first;
        S second;

        public Pair(F first, S second) {
            this.first = first;
            this.second = second;
        }
    }

    public abstract static class Node {
        protected final ArrayList<Node> in;
        protected ArrayList<ArrayList<Double>> values;
        protected ArrayList<ArrayList<Double>> differentials;

        public Node(ArrayList<Node> nodes) {
            this.in = new ArrayList<>(nodes);
        }

        public abstract void get();

        public abstract void diff();

        public void initializeDifferentials() {
            differentials = generate(values.size(), values.get(0).size());
        }

        public void fillDifferentials(FastScannerSecond reader) {
            differentials.forEach(i -> i.replaceAll(ignored -> {
                try {
                    return (double) reader.nextInt();
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            }));
        }

        private void printSome(PrintWriter printWriter, ArrayList<ArrayList<Double>> printed) {
            for (ArrayList<Double> i : printed) {
                for (Double j : i) {
                    printWriter.print(String.format("%.20f ", j));
                }
                printWriter.println();
            }
        }

        public void printNode(PrintWriter printWriter) {
            printSome(printWriter, values);
        }

        public void printDifferentials(PrintWriter printWriter) {
            printSome(printWriter, differentials);
        }
    }

    public static class Var extends Node {

        public Var() {
            super(new ArrayList<>());
        }

        public void set(ArrayList<ArrayList<Double>> d) {
            values = cloneAA(d);
        }

        @Override
        public void get() {
            initializeDifferentials();
        }

        @Override
        public void diff() {
        }
    }

    public static class Tnh extends Node {

        public Tnh(Node node) {
            super(new ArrayList<>(Collections.singleton(node)));
        }

        @Override
        public void get() {
            values = cloneAA(in.get(0).values);
            for (ArrayList<Double> i : values) {
                i.replaceAll(Math::tanh);
            }
            initializeDifferentials();
        }

        @Override
        public void diff() {
            for (int i = 0; i < values.size(); i++) {
                for (int j = 0; j < values.get(i).size(); j++) {
                    in.get(0).differentials.get(i).set(j,
                            in.get(0).differentials.get(i).get(j) + differentials.get(i).get(j) * (1 - Math.pow(values.get(i).get(j), 2)));
                }
            }
        }
    }

    public static class Rlu extends Node {

        double invAlpha;

        public Rlu(double invAlpha, Node node) {
            super(new ArrayList<>(Collections.singleton(node)));
            this.invAlpha = invAlpha;
        }

        @Override
        public void get() {
            values = cloneAA(in.get(0).values);
            for (ArrayList<Double> i : values) {
                for (int j = 0; j < i.size(); j++) {
                    if (i.get(j) < 0) {
                        i.set(j, i.get(j) / invAlpha);
                    }
                }
            }
            initializeDifferentials();
        }

        @Override
        public void diff() {
            for (int i = 0; i < values.size(); i++) {
                for (int j = 0; j < values.get(i).size(); j++) {
                    double current = in.get(0).values.get(i).get(j);
                    double koff;
                    if (current >= 0) {
                        koff = 1;
                    } else {
                        koff = 1 / invAlpha;
                    }
                    in.get(0).differentials.get(i).set(j, in.get(0).differentials.get(i).get(j) + koff * differentials.get(i).get(j));
                }
            }
        }
    }

    public static class Mul extends Node {

        public Mul(Node a1, Node a2) {
            super(new ArrayList<>(List.of(a1, a2)));
        }

        @Override
        public void get() {
            ArrayList<ArrayList<Double>> first = in.get(0).values;
            ArrayList<ArrayList<Double>> second = in.get(1).values;
            int m = first.get(0).size();
            int k = second.get(0).size();
            values = generate(first.size(), k);
            for (int i = 0; i < first.size(); i++) {
                for (int j = 0; j < k; j++) {
                    for (int l = 0; l < m; l++) {
                        values.get(i).set(j, values.get(i).get(j) + first.get(i).get(l) * second.get(l).get(j));
                    }
                }
            }
            initializeDifferentials();
        }

        @Override
        public void diff() {
            ArrayList<ArrayList<Double>> first = in.get(0).values;
            ArrayList<ArrayList<Double>> second = in.get(1).values;
            ArrayList<ArrayList<Double>> dfirst = in.get(0).differentials;
            ArrayList<ArrayList<Double>> dsecond = in.get(1).differentials;
            int n = first.size();
            int m = first.get(0).size();
            int k = second.get(0).size();
            for (int i = 0; i < n; i++) {
                for (int j = 0; j < m; j++) {
                    double cur_diff = 0;
                    for (int t = 0; t < k; ++t) {
                        cur_diff += differentials.get(i).get(t) * second.get(j).get(t);
                    }
                    dfirst.get(i).set(j, dfirst.get(i).get(j) + cur_diff);
                }
            }
            for (int i = 0; i < m; i++) {
                for (int j = 0; j < k; j++) {
                    double cur_diff = 0;
                    for (int t = 0; t < n; ++t) {
                        cur_diff += differentials.get(t).get(j) * first.get(t).get(i);
                    }
                    dsecond.get(i).set(j, dsecond.get(i).get(j) + cur_diff);
                }
            }

        }
    }

    public static class Sum extends Node {

        public Sum(ArrayList<Node> nodes) {
            super(nodes);
        }

        @Override
        public void get() {
            int n = in.get(0).values.size();
            int m = in.get(0).values.get(0).size();
            values = generate(n, m);
            for (Node node : in) {
                ArrayList<ArrayList<Double>> current = node.values;
                for (int i = 0; i < n; ++i) {
                    for (int j = 0; j < m; ++j) {
                        values.get(i).set(j, values.get(i).get(j) + current.get(i).get(j));
                    }
                }
            }
            initializeDifferentials();
        }

        @Override
        public void diff() {
            for (int i = 0; i < values.size(); ++i) {
                for (int j = 0; j < values.get(0).size(); ++j) {
                    for (Node node : in) {
                        node.differentials.get(i).set(j, node.differentials.get(i).get(j) + differentials.get(i).get(j));
                    }
                }
            }
        }
    }

    public static class Had extends Node {

        public Had(ArrayList<Node> nodes) {
            super(nodes);
        }

        @Override
        public void get() {
            int n = in.get(0).values.size();
            int m = in.get(0).values.get(0).size();
            values = generate(n, m, 1.0);
            for (Node input : in) {
                ArrayList<ArrayList<Double>> current = input.values;
                for (int i = 0; i < n; ++i) {
                    for (int j = 0; j < m; ++j) {
                        values.get(i).set(j, values.get(i).get(j) * current.get(i).get(j));
                    }
                }
            }
            initializeDifferentials();
        }

        @Override
        public void diff() {
            for (int i = 0; i < values.size(); ++i) {
                for (int j = 0; j < values.get(0).size(); ++j) {
                    for (int k = 0; k < in.size(); ++k) {
                        double koff = 1;
                        for (int t = 0; t < in.size(); ++t) {
                            if (t != k) {
                                koff *= in.get(t).values.get(i).get(j);
                            }
                        }
                        in.get(k).differentials.get(i).set(j, in.get(k).differentials.get(i).get(j) + koff * differentials.get(i).get(j));
                    }
                }
            }
        }
    }

    public static class Network {
        ArrayList<Node> nodes = new ArrayList<>();

        public ArrayList<Node> getPrevious(ArrayList<Integer> indexes) {
            ArrayList<Node> returned = new ArrayList<>();
            for (int index : indexes) {
                returned.add(nodes.get(index));
            }
            return returned;
        }

        public void add(Node node) {
            nodes.add(node);
        }

        public void printNode(int index, PrintWriter printWriter) {
            nodes.get(index).printNode(printWriter);
        }

        public void printDifferentials(int index, PrintWriter printWriter) {
            nodes.get(index).printDifferentials(printWriter);
        }

        public void getAll() {
            nodes.forEach(Node::get);
        }

        public void diff() {
            for (int i = nodes.size() - 1; i >= 0; i--) {
                nodes.get(i).diff();
            }
        }
    }

    public static ArrayList<ArrayList<Double>> generate(int n, int m) {
        return generate(n, m, 0.0);
    }

    public static ArrayList<ArrayList<Double>> generate(int n, int m, double x) {
        ArrayList<ArrayList<Double>> returned = new ArrayList<>();
        IntStream.range(0, n).forEach(i -> {
            returned.add(new ArrayList<>());
            IntStream.range(0, m).forEach(j ->
                    returned.get(i).add(x));
        });
        return returned;
    }

    public static void main(String[] args) throws IOException {
        FastScannerSecond fs = new FastScannerSecond(System.in);
        PrintWriter printWriter = new PrintWriter(System.out);
        int n = fs.nextInt();
        int m = fs.nextInt();
        int k = fs.nextInt();
        ArrayList<Pair<Integer, Integer>> sizes = new ArrayList<>();
        Network network = new Network();
        for (int i = 0; i < n; i++) {
            String type;
            type = fs.nextWord();

            switch (type) {
                case "var" -> {
                    int a = fs.nextInt();
                    int b = fs.nextInt();
                    sizes.add(new Pair<>(a, b));
                    network.add(new Var());
                }
                case "tnh" -> {
                    int c = fs.nextInt();
                    network.add(new Tnh(network.nodes.get(c - 1)));
                }
                case "rlu" -> {
                    int d = fs.nextInt();
                    int e = fs.nextInt();
                    network.add(new Rlu(d, network.nodes.get(e - 1)));
                }
                case "mul" -> {
                    int f = fs.nextInt();
                    int g = fs.nextInt();
                    network.add(new Mul(network.nodes.get(f - 1), network.nodes.get(g - 1)));
                }
                case "sum" -> {
                    int length = fs.nextInt();
                    ArrayList<Integer> nodes = new ArrayList<>();
                    for (int j = 0; j < length; j++) {
                        nodes.add(fs.nextInt() - 1);
                    }
                    network.add(new Sum(network.getPrevious(nodes)));
                }
                case "had" -> {
                    int length_1 = fs.nextInt();
                    ArrayList<Integer> nodes_1 = new ArrayList<>();
                    for (int j = 0; j < length_1; j++) {
                        nodes_1.add(fs.nextInt() - 1);
                    }
                    network.add(new Had(network.getPrevious(nodes_1)));
                }
                default -> throw new IllegalStateException("");
            }
        }
        for (int i = 0; i < m; i++) {
            ArrayList<ArrayList<Double>> temp = generate(sizes.get(i).first, sizes.get(i).second);
            for (int j = 0; j < sizes.get(i).first; j++) {
                for (int kk = 0; kk < sizes.get(i).second; kk++) {
                    temp.get(j).set(kk, (double) fs.nextInt());
                }
            }
            if (network.nodes.get(i) instanceof Var) {
                ((Var) network.nodes.get(i)).set(temp);
            } else {
                throw new IllegalStateException("");
            }
        }
        network.getAll();
        for (int i = n - k; i < n; i++) {
            network.printNode(i, printWriter);
        }
        for (int i = n - k; i < n; i++) {
            network.nodes.get(i).fillDifferentials(fs);
        }
        network.diff();
        for (int i = 0; i < m; i++) {
            network.printDifferentials(i, printWriter);
        }

        printWriter.close();
    }

    public static class FastScannerSecond implements Closeable {

        private final Reader reader;
        private final String lineSeparator = System.lineSeparator();

        private final char[] buffer = new char[1024];
        private int pos = 0;
        private int size = 0;
        private Integer rollback = null;

        enum Token {
            INT,
            WORD,
            LINE
        }


        public FastScannerSecond(InputStream in) {
            reader = new InputStreamReader(in);
        }

        public FastScannerSecond(String in) {
            reader = new StringReader(in);
        }

        public FastScannerSecond(File in) throws IOException {
            reader = new FileReader(in, StandardCharsets.UTF_8);
        }

        public boolean hasNextChar() throws IOException {
            return pos < size || read();
        }

        public char nextChar() throws IOException {
            if (hasNextChar()) {
                return buffer[pos++];
            }
            throw new NoSuchElementException("Has no next Character.");
        }

        public boolean hasNextLine() throws IOException {
            return hasNextChar();
        }

        public boolean hasNextInt() throws IOException {
            rollback = pos;
            boolean res = nextToken(Token.INT) != null;
            pos = rollback;
            rollback = null;
            return res;
        }

        public boolean hasNextWord() throws IOException {
            while (hasNextChar() && Character.isWhitespace(buffer[pos])) {
                pos++;
            }
            return hasNextChar();
        }

        public String nextLine() throws IOException {
            String res = nextToken(Token.LINE);
            if (res == null) {
                throw new InputMismatchException("Has no next Line.");
            }
            return res;
        }

        public String nextWord() throws IOException {
            String res = nextToken(Token.WORD);
            if (res == null) {
                throw new InputMismatchException("Has no next Word.");
            }
            return res;
        }

        public Integer nextInt() throws IOException {
            String res = nextToken(Token.INT);
            if (res == null) {
                throw new InputMismatchException("Has no next Int.");
            }
            return Integer.parseInt(res);
        }

        private String nextToken(Token token) throws IOException {
            if (!prevCheck(token)) {
                return null;
            }
            StringBuilder sb = new StringBuilder();
            while (hasNextChar() && checkLetter(buffer[pos], sb.length(), token)) {
                sb.append(buffer[pos]);
                pos++;
            }
            Character c = null;
            if (hasNextChar()) {
                c = buffer[pos];
            }
            try {
                return checkAndGetResult(sb.toString(), c, token);
            } finally {
                postAction(token);
            }
        }

        private void postAction(Token token) throws IOException {
            switch (token) {
                case LINE -> {
                    int i = 0;
                    while (i < lineSeparator.length() && hasNextChar() && lineSeparator.charAt(i) == buffer[pos]) {
                        pos++;
                        i++;
                    }
                    if (i < lineSeparator.length()) {
                        throw new InputMismatchException("Wrong line separator.");
                    }
                }
                case INT, WORD -> {
                }
                default -> throw new IllegalStateException("Unexpected value: " + token);
            }
        }


        private boolean prevCheck(Token token) throws IOException {
            switch (token) {
                case WORD, INT -> {
                    if (hasNextWord()) return true;
                }
                case LINE -> {
                    if (hasNextLine()) return true;
                }
                default -> throw new IllegalStateException("Unexpected value: " + token);
            }
            return false;
        }

        private boolean checkLetter(char c, int posInTokenValue, Token token) {
            boolean res;
            switch (token) {
                case INT -> {
                    res = posInTokenValue == 0 && (c == '-' || c == '+');
                    res = res || Character.isDigit(c);
                    res = res && posInTokenValue < 12;
                }
                case WORD -> res = !Character.isWhitespace(c);
                case LINE -> res = c != lineSeparator.charAt(0);
                default -> throw new IllegalStateException("Unexpected value: " + token);
            }
            return res;
        }

        private String checkAndGetResult(String str, Character trailing, Token token) {
            switch (token) {
                case INT -> {
                    if (str.length() == 0 || trailing != null && !Character.isWhitespace(trailing) ||
                            (str.charAt(0) == '-' || str.charAt(0) == '+') && str.length() == 1) {
                        return null;
                    }
                }
                case WORD -> {
                    if (str.length() == 0 || trailing != null && !Character.isWhitespace(trailing)) {
                        return null;
                    }
                }
                case LINE -> {
                    if (trailing != null && trailing != lineSeparator.charAt(0)) {
                        return null;
                    }
                }
                default -> throw new IllegalStateException("Unexpected value: " + token);
            }
            return str;
        }


        @Override
        public void close() throws IOException {
            reader.close();
        }

        private boolean read() throws IOException {
            if (rollback == null) {
                size = reader.read(buffer);
                pos = 0;
                return size != -1;
            }
            if (rollback != 0) {
                moveToBeginning();
            }
            int readLength = reader.read(buffer, size, buffer.length - size);
            if (readLength == -1) {
                return false;
            }
            size += readLength;
            return true;
        }

        private void moveToBeginning() {
            for (int i = rollback, j = 0; i < buffer.length; i++, j++) {
                buffer[j] = buffer[i];
            }
            size -= rollback;
            pos -= rollback;
            rollback = 0;
        }
    }
}
