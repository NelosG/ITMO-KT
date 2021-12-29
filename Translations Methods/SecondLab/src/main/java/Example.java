import parser.Parser;
import parser.ParserException;
import tree.Tree;

import java.io.IOException;

public class Example {
    public static void main(String[] args) throws ParserException, IOException {
        String text = "int *a;";
        Tree.render(new Parser().parse(text), "./images/example.png");
    }
}
