import java.util.ArrayList;
import java.util.Arrays;
import java.util.Scanner;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class J_LSTM {
    private static Scanner sc;

    private static ArrayList<Double> getArray(int size, double value) {
        return Stream.iterate(value, i -> i)
                .limit(size)
                .collect(Collectors.toCollection(ArrayList::new));
    }

    private static ArrayList<ArrayList<Double>> getArray2(int size1, int size2, double value) {
        return Stream.iterate(0., i -> i)
                .limit(size1)
                .map(k -> getArray(size2, value))
                .collect(Collectors.toCollection(ArrayList::new));
    }

    private abstract static class Node {

        Network network;

        ArrayList<Integer> indexes;
        ArrayList<ArrayList<Double>> value;
        ArrayList<ArrayList<Double>> diff;

        public Node getInput(int index) {
            return network.nodes.get(indexes.get(index));
        }

        abstract void compute();

        abstract void spread_diff();

        void initialize_diff() {
            diff = getArray2(value.size(), value.get(0).size(), 0);
        }

        void read_diff() {
            for (ArrayList<Double> doubles : diff) {
                doubles.replaceAll(ignored -> (double)sc.nextInt());
            }
        }

        Node(Network net, ArrayList<Integer> indexes) {
            this.network = net;
            this.indexes = indexes;
            value = new ArrayList<>();
        }
    }

    private static class Var extends Node {
        Var(Network net) {
            super(net, new ArrayList<>());
        }

        void set_data(ArrayList<ArrayList<Double>> data) {
            value = data;
        }

        void compute() {
            initialize_diff();
        }

        void spread_diff() {
        }
    }

    private static class Tnh extends Node {
        Tnh(Network net, Integer source) {
            super(net, new ArrayList<>());
            indexes.add(source);
        }

        void compute() {
            value = getInput(0).value;
            for (ArrayList<Double> doubles : value) {
                doubles.replaceAll(Math::tanh);
            }
            initialize_diff();
        }

        void spread_diff() {
            for (int i = 0; i < value.size(); ++i) {
                for (int j = 0; j < value.get(0).size(); ++j) {
                    var cur_value = value.get(i).get(j);
                    getInput(0).diff.get(i).set(j,
                            getInput(0).diff.get(i).get(j) +
                                    (1 - cur_value * cur_value) * diff.get(i).get(j));
                }
            }
        }
    }

    private static class Sigm extends Node {
        Sigm(Network net, Integer source) {
            super(net, new ArrayList<>());
            indexes.add(source);
        }

        void compute() {
            value = getInput(0).value;
            for (ArrayList<Double> doubles : value) {
                doubles.replaceAll(cell -> 1.0 / (1 + Math.exp(-cell)));
            }
            initialize_diff();
        }

        void spread_diff() {
            for (int i = 0; i < value.size(); ++i) {
                for (int j = 0; j < value.get(0).size(); ++j) {
                    var cur_value = value.get(i).get(j);
                    getInput(0).diff.get(i).set(j,
                            getInput(0).diff.get(i).get(j) +
                                    cur_value * (1 - cur_value) * diff.get(i).get(j));
                }
            }
        }
    }

    private static class Mul extends Node {
        Mul(Network net, Integer a, Integer b) {
            super(net, new ArrayList<>());
            indexes.add(a);
            indexes.add(b);
        }

        void compute() {
            var a = getInput(0).value;
            var b = getInput(1).value;
            var n = a.size();
            var m = a.get(0).size();
            var k = b.get(0).size();

            value = getArray2(n, k, 0);

            for (int i = 0; i < n; ++i) {
                for (int j = 0; j < k; ++j) {
                    for (int t = 0; t < m; ++t) {
                        value.get(i).set(j,
                                value.get(i).get(j) +
                                a.get(i).get(t) * b.get(t).get(j));
                    }
                }
            }
            initialize_diff();
        }

        void spread_diff() {
            var a = getInput(0).value;
            var b = getInput(1).value;
            var n = a.size();
            var m = a.get(0).size();
            var k = b.get(0).size();
            for (int i = 0; i < n; ++i) {
                for (int j = 0; j < m; ++j) {
                    double cur_diff = 0;
                    for (int t = 0; t < k; ++t) {
                        cur_diff += diff.get(i).get(t) * b.get(j).get(t);
                    }
                    getInput(0).diff.get(i).set(j,
                            getInput(0).diff.get(i).get(j) + cur_diff);
                }
            }
            for (int i = 0; i < m; ++i) {
                for (int j = 0; j < k; ++j) {
                    double cur_diff = 0;
                    for (int t = 0; t < n; ++t) {
                        cur_diff += a.get(t).get(i) * diff.get(t).get(i);
                    }
                    getInput(1).diff.get(i).set(j,
                            getInput(1).diff.get(i).get(j) + cur_diff);
                }
            }
        }
    }

    private static class Sum extends Node {

        Sum(Network net, ArrayList<Integer> inputs) {
            super(net,inputs);
        }

        void compute() {
            var n = getInput(0).value.size();
            var m = getInput(0).value.get(0).size();
            value = getArray2(n, m, 0);

            for (int input = 0; input < indexes.size(); input++) {
                for (int i = 0; i < n; i++) {
                    for (int j = 0; j < m; j++) {
                        value.get(i).set(j,
                                value.get(i).get(j) + getInput(input).value.get(i).get(j));
                    }
                }
            }
            initialize_diff();
        }

        void spread_diff() {
            for (int i = 0; i < value.size(); ++i) {
                for (int j = 0; j < value.get(i).size(); ++j) {
                    for (int input = 0; input < indexes.size(); input++) {
                        getInput(input).diff.get(i).set(j,
                                getInput(input).diff.get(i).get(j) + diff.get(i).get(j));
                    }
                }
            }
        }
    }


    private static class Had extends Node {

        Had(Network net, ArrayList<Integer> inputs) {
            super(net, inputs);
        }

        void compute() {
            var n = getInput(0).value.size();
            var m = getInput(0).value.get(0).size();
            value = getArray2(n, m, 1);

            for (int input = 0; input < indexes.size(); input++) {
                for (int i = 0; i < n; i++) {
                    for (int j = 0; j < m; j++) {
                        value.get(i).set(j,
                                value.get(i).get(j) * getInput(input).value.get(i).get(j));
                    }
                }
            }
            initialize_diff();
        }

        void spread_diff() {
            for (int i = 0; i < value.size(); ++i) {
                for (int j = 0; j < value.get(0).size(); ++j) {
                    for (int k = 0; k < indexes.size(); ++k) {
                        double multiplier = 1;
                        for (int t = 0; t < indexes.size(); ++t) {
                            if (t != k) {
                                multiplier *= getInput(t).value.get(i).get(j);
                            }
                        }
                        getInput(k).diff.get(i).set(j,
                                getInput(k).diff.get(i).get(j) + multiplier * diff.get(i).get(j));
                    }
                }
            }
        }
    }


    private static class Network {
        ArrayList<Node> nodes = new ArrayList<>();

        Node get_node(int pos) {
            return nodes.get(pos);
        }

        Integer add_node(Node new_node) {
            nodes.add(new_node);
            return nodes.size() - 1;
        }

        void print_node(int idx) {
            for (var row : nodes.get(idx).value) {
                for (var cell : row) {
                    System.out.print(cell + " ");
                }
                System.out.println();
            }
        }

        void print_diff(int idx) {
            for (var row : nodes.get(idx).diff) {
                for (var cell : row) {
                    System.out.print(cell + " ");
                }
                System.out.println();
            }
        }

        void compute() {
            for (Node node : nodes) {
                node.compute();
            }
        }

        void backprop() {
            for (int i = nodes.size() - 1; i >= 0; i--) {
                nodes.get(i).spread_diff();
            }
        }
    }

    ;

    public static void main(String[] args) {
        sc = new Scanner(System.in);


        Network net = new Network();
        int n = sc.nextInt();
        for (int i = 0; i < 4; ++i) {
            for (int t = 0; t < 2; ++t) {
                ArrayList<ArrayList<Double>> matr = getArray2(n, n, 0);
                for (int j = 0; j < n; ++j) {
                    for (int k = 0; k < n; ++k) {
                        matr.get(j).set(k, (double)sc.nextInt());
                    }
                }
                Var matr_node = new Var(net);
                matr_node.set_data(matr);
                net.add_node(matr_node);
            }
            ArrayList<ArrayList<Double>> b = getArray2(n, 1, 0);
            for (int j = 0; j < n; ++j) {
                b.get(j).set(0, (double)sc.nextInt());
            }
            var b_node = new Var(net);
            b_node.set_data(b);
            net.add_node(b_node);
        }
        int m = sc.nextInt();
        for (int i = 0; i < 2; ++i) {
            ArrayList<ArrayList<Double>> vec = getArray2(n, 1, 0);
            for (int j = 0; j < n; ++j) {
                vec.get(j).set(0, (double)sc.nextInt());
            }
            var vec_node = new Var(net);
            vec_node.set_data(vec);
            net.add_node(vec_node);
        }
        int NODES_START = 4 * 3 + 2;
        int NODE_SIZE = 1 + 3 * 4 + 8;
        var temp = net.nodes.get(NODES_START - 1);
        net.nodes.set(NODES_START - 1, net.nodes.get(NODES_START - 2));
        net.nodes.set(NODES_START - 2, temp);

        ArrayList<Integer> os = new ArrayList<>();
        os.add(-1_000_000_000);
        ArrayList<Integer> cs = new ArrayList<>();
        cs.add(NODES_START - 2);
        ArrayList<Integer> hs = new ArrayList<>();
        hs.add(NODES_START - 1);
        ArrayList<Integer> xs = new ArrayList<>();
        xs.add(-1_000_000_000);


        for (int i = 0; i < m; ++i) {
            ArrayList<ArrayList<Double>> input = getArray2(n, 1, 0);
            for (int j = 0; j < n; ++j) {
                input.get(j).set(0, (double)sc.nextInt());
            }
            var input_node = new Var(net);
            input_node.set_data(input);
            int input_node_id = net.add_node(input_node);
            int start_pos = NODES_START + NODE_SIZE * i;
            int prev_h_pos = start_pos - 1;
            int prev_c_pos = start_pos - 2;
            int[] sums = new int[4];
            for (int j = 0; j < 4; ++j) {
                int Wx_pos = net.add_node(new Mul(net,j * 3, input_node_id));
                int Uh_pos = net.add_node(new Mul(net, j * 3 + 1, prev_h_pos));
                sums[j] = net.add_node(new Sum(net, new ArrayList<>(Arrays.asList(Wx_pos, Uh_pos, j * 3 + 2))));
            }
            int f_pos = net.add_node(new Sigm(net, sums[0]));
            int i_pos = net.add_node(new Sigm(net, sums[1]));
            int o_pos = net.add_node(new Sigm(net, sums[2]));
            int tahn_pos = net.add_node(new Tnh(net, sums[3]));
            int ithan_pos = net.add_node(new Had(net,new ArrayList<>(Arrays.asList(i_pos, tahn_pos))));
            int fprev_c_pos = net.add_node(new Had(net,new ArrayList<>(Arrays.asList(f_pos, prev_c_pos))));
            int cur_c_pos = net.add_node(new Sum(net,new ArrayList<>(Arrays.asList(fprev_c_pos, ithan_pos))));
            int cur_h_pos = net.add_node(new Had(net,new ArrayList<>(Arrays.asList(o_pos, cur_c_pos))));
            os.add(o_pos);
            cs.add(cur_c_pos);
            hs.add(cur_h_pos);
            xs.add(start_pos);
        }

        net.compute();
        net.get_node(hs.get(m)).read_diff();
        net.get_node(cs.get(m)).read_diff();
        for (int i = m; i >= 1; --i) {
            net.get_node(os.get(i)).read_diff();
        }
        net.backprop();

        for (int i = 1; i <= m; ++i) {
            net.print_node(os.get(i));
        }
        net.print_node(hs.get(m));
        net.print_node(cs.get(m));
        for (int i = m; i >= 1; --i) {
            net.print_diff(xs.get(i));
        }
        net.print_diff(hs.get(0));
        net.print_diff(cs.get(0));
        for (int i = 0; i < 4 * 3; ++i) {
            net.print_diff(i);
        }
        sc.close();
    }
}
