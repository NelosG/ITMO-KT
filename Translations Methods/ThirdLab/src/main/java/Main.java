import generated.JavaHighlightLexer;
import generated.JavaHighlightParser;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.tree.ParseTree;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;

public class Main {
    public static void main(String[] args) throws IOException {
        String prefix = "./src/main/resources/Java/";
        highlight("src/main/java/CustomJavaVisitor.java",
                prefix + "index.html");
        highlight(prefix + "all.txt",
                prefix + "all.html");

        highlight(prefix + "RealClass.txt",
                prefix + "RealClass.html");
    }

    private static void highlight(String input, String output) throws IOException {
        Lexer lexer = new JavaHighlightLexer(
                CharStreams.fromPath(Path.of(input), StandardCharsets.UTF_8));
        ParseTree tree = new JavaHighlightParser(new CommonTokenStream(lexer)).program();
        StringBuilder result = new CustomJavaVisitor().visit(tree);
        BufferedWriter writer = new BufferedWriter(new FileWriter(output));
        writer.write(result.toString());
        writer.close();
    }
}
