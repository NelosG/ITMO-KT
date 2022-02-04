package parser;

import org.junit.Test;
import tree.Tree;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.stream.Collectors;

import static org.junit.Assert.assertEquals;

public class ParserTests {

    private static String message(String expected, String actual) {
        return String.format("Expected:[%s], Actual:[%s]", expected, actual);
    }

    @Test
    public void BaseTest1() throws ParserException {
        String text = """
                int*a;""";
        String actual = Tree.collectIntoOriginalExpression(new Parser().parse(text));
        assertEquals(message(text, actual), text, actual);
    }

    @Test
    public void BaseTest2() throws ParserException {
        String text = """
                int * a;
                """;
        String expected = """
                S0 -> Type0
                Type0 -> "int"
                S0 -> D0
                D0 -> T0
                T0 -> "*"
                T0 -> T1
                T1 -> Var0
                Var0 -> "a"
                S0 -> L0
                L0 -> ";"
                """;
        String actual = Tree.print(new Parser().parse(text));
        assertEquals(message(expected, actual), expected, actual);
    }

    @Test
    public void HardTest1() throws ParserException {
        String text = """
                   int\040
                    *
                *     a,\040
                * * * ***b\040
                , c;;
                ;
                ;
                 ;\040
                   int\040
                    *
                *     a,\040
                * * * ***b\040
                , c
                ; ; ;
                                
                """;
        String expected = "int**a,******b,c;;;;;int**a,******b,c;;;";
        String actual = Tree.collectIntoOriginalExpression(new Parser().parse(text));
        assertEquals(message(expected, actual), expected, actual);
    }

    @Test
    public void HardTest2() throws ParserException, IOException {
        String text = """
                   int\040
                    *
                *     a,\040
                * * * ***b\040
                , c;;
                ;
                ;
                 ;\040
                   int\040
                    *
                *     a,\040
                * * * ***b\040
                , c
                ; ; ;
                                
                """;
        String expected = Files.readAllLines(Path.of("./src/test/java/parser/HardTestExpected.txt")).stream().
                map(s -> s + '\n').collect(Collectors.joining());
        String actual = Tree.print(new Parser().parse(text));
        assertEquals(message(expected, actual), expected, actual);
    }
}
