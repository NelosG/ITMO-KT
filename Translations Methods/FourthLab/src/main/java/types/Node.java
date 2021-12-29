package types;

import java.util.ArrayList;
import java.util.List;

public class Node {
    private final List<Node> nodes = new ArrayList<>();
    private final String value;

    public Node(String tokenOrValue) {
        value = tokenOrValue;
    }

    public List<Node> getNodes() {
        return nodes;
    }

    public String getValue() {
        return value;
    }

    public Node add(Node... node) {
        if (node != null) {
            for (Node n : node) {
                if (n != null) {
                    nodes.add(n);
                }
            }
        }
        return this;
    }
}
