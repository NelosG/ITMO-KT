import builder.Builder;
import generated.GrammarLexer;
import generated.GrammarParser;
import generators.LexerGenerator;
import generators.ParserGenerator;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.ParseTreeWalker;

import java.io.FileWriter;
import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Path;

public class Main {
    public static void main(String[] args) {

        for (var variant : new String[]{"Calc", "CArgs", "JavaHighlight"}) {
            try {

                Builder builder = newGrammarBuilder(variant);
                LexerGenerator lexerGenerator = new LexerGenerator(builder);
                ParserGenerator parserGenerator = new ParserGenerator(builder);
                FileWriter lexerWriter = new FileWriter("src/test/java/" +
                        variant.toLowerCase() + "/" +
                        lexerGenerator.getLexerName() + ".java",
                        StandardCharsets.UTF_8);
                lexerWriter.write(
                        lexerGenerator.generate(variant)
                                .build()
                                .toString());
                lexerWriter.close();

                FileWriter parserWriter = new FileWriter("src/test/java/" +
                        variant.toLowerCase() + "/" +
                        parserGenerator.getParserName() + ".java",
                        StandardCharsets.UTF_8);
                parserWriter.write(
                        parserGenerator.generate(variant)
                                .build()
                                .toString());
                parserWriter.close();
            } catch (IOException e) {
                System.err.println(variant + " : OOOps...\n" + e.getMessage());
            }
        }

    }

    private static void printFirstFollow(ParserGenerator parserGenerator) {
        System.out.println("____________FIRST_______________");
        parserGenerator.first.forEach((key, value) -> {
            System.out.println(key + " " + value);

        });

        System.out.println("____________FOLLOW_______________");
        parserGenerator.follow.forEach((key, value) -> {
            System.out.println(key + " " + value);

        });
    }

    private static Builder newGrammarBuilder(String grammarName) throws IOException {
        Lexer lexer = new GrammarLexer(
                CharStreams.fromPath(
                        Path.of(
                                "src/test/java/" +
                                        grammarName.toLowerCase() + "/" +
                                        grammarName + ".grammar"
                        ),
                        StandardCharsets.UTF_8
                )
        );
        ParseTree tree = new GrammarParser(new CommonTokenStream(lexer)).start();

        ParseTreeWalker walker = new ParseTreeWalker();
        Builder listener = new Builder(grammarName);

        walker.walk(listener, tree);

        return listener;
    }
}
