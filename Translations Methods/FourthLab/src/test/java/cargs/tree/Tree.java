package cargs.tree;

import guru.nidi.graphviz.attribute.Label;
import guru.nidi.graphviz.engine.Format;
import guru.nidi.graphviz.engine.Graphviz;
import guru.nidi.graphviz.engine.Renderer;
import guru.nidi.graphviz.model.Graph;
import types.Node;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import static guru.nidi.graphviz.model.Factory.graph;
import static guru.nidi.graphviz.model.Factory.node;

public class Tree {

    public static String print(Node node) {
        return print(new HashMap<>(), node);
    }

    private static String print(Map<String, Integer> map, Node ourNode) {
        StringBuilder sb = new StringBuilder();
        if(ourNode == null) {
            return "";
        }
        String value = ourNode.getValue();

        map.compute(value, (k, v) -> v == null ? 0 : (v + 1));

        for (Node node : ourNode.getNodes()) {
            Integer nodeValueN = map.get(node.getValue());
            sb.append(value).append(map.get(value)).append(" -> ").append(node.getValue());
            if (node.getValue().charAt(0) != '\"') {
                sb.append(nodeValueN == null ? 0 : nodeValueN + 1);
            }
            sb.append('\n').append(print(map, node));
        }
        return sb.toString();
    }

    public static String collectIntoOriginalExpression(Node ourNode) {
        String nodeValue = ourNode.getValue();
        if (nodeValue.charAt(0) == '\"') {
            if (nodeValue.charAt(1) == '\\' && nodeValue.charAt(2) == 'n') {
                return "\n";
            }
            return nodeValue.substring(1, nodeValue.length() - 1);
        }
        StringBuilder sb = new StringBuilder();
        for (Node node : ourNode.getNodes()) {
            sb.append(collectIntoOriginalExpression(node));
        }
        return sb.toString();
    }

    public static void render(Node node, String path) throws IOException {
        renderImpl(node).toFile(new File(path));
    }

    public static BufferedImage render(Node node) {
        return renderImpl(node).toImage();
    }

    private static Renderer renderImpl(Node node) {
        List<guru.nidi.graphviz.model.Node> list = renderList(new HashMap<>(), node, new ArrayList<>());
        Graph g = graph("graph").directed()
                .linkAttr().with("class", "link-class")
                .with(list);
        return Graphviz.fromGraph(g).height(Math.max(Math.min(list.size() * 50, 2048), 500)).render(Format.PNG);
    }

    private static List<guru.nidi.graphviz.model.Node> renderList(Map<String, Integer> map, Node ourNode,
                                                                  List<guru.nidi.graphviz.model.Node> list) {
        String value = ourNode.getValue();

        map.compute(value, (k, v) -> v == null ? 0 : (v + 1));
        if (ourNode.getNodes().isEmpty()) {
            return list;
        }
        for (Node node : ourNode.getNodes()) {
            Integer nodeValueN = map.get(node.getValue());
            String nodeValue = node.getValue();
            list.add(node(value + map.get(value))
                    .with(Label.of(value)).link(node(nodeValue + (nodeValueN == null ? 0 : nodeValueN + 1))
                            .with(Label.of(nodeValue))));
            renderList(map, node, list);
        }
        return list;
    }
}
