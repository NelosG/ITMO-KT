import org.junit.Test;
import parser.Parser;
import parser.ParserException;
import tree.Tree;

import java.util.Date;
import java.util.Random;

import static org.junit.Assert.assertEquals;

public class RandomTests {
    private final String chars = "_abcdefghijklmnopqrstuvwxyz1234567890$";
    private final String whiteSpaces = "\n\n\t ";
    private final Random random = new Random(new Date().getTime());
    private StringBuilder withoutWS;

    @Test
    public void BaseTest1() throws ParserException {
        for (int i = 0; i < 100_000; i++) {
            withoutWS = new StringBuilder();
            String text = generateExpr();
            String actual = Tree.collectIntoOriginalExpression(new Parser().parse(text));
            assertEquals(withoutWS.toString(), actual);
        }
    }


    private String generateExpr() {
        StringBuilder out = new StringBuilder(generateSpaces());
        out.append(generateTypeOrVariable())
                .append(generateSpacesPlus())
                .append(generateVariable())
                .append(generateSpaces());
        for (int i = 0; i < random.nextInt(100); i++) {
            withoutWS.append(',');
            out.append(',')
                    .append(generateSpaces())
                    .append(generateVariable())
                    .append(generateSpaces());
        }
        return out.append(generateSemilicon()).toString();
    }

    private String generateSemilicon() {
        StringBuilder out = new StringBuilder();
        for (int i = 0; i < random.nextInt(10) + 1; i++) {
            withoutWS.append(';');
            out.append(generateSpaces())
                    .append(';');
        }
        return out.toString();
    }

    private String generateVariable() {
        return generateStars() + generateTypeOrVariable();
    }

    private String generateStars() {
        StringBuilder out = new StringBuilder(generateSpaces());
        for (int i = 0; i < random.nextInt(20); i++) {
            withoutWS.append('*');
            out.append('*').append(generateSpaces());
        }
        return out.toString();
    }

    private String generateSpacesPlus() {
        return generateSpaces() + whiteSpaces.charAt(random.nextInt(whiteSpaces.length()));
    }

    private String generateSpaces() {
        StringBuilder out = new StringBuilder();
        for (int i = 0; i < random.nextInt(20); i++) {
            out.append(whiteSpaces.charAt(random.nextInt(whiteSpaces.length())));
        }
        return out.toString();
    }


    private String generateTypeOrVariable() {
        char firstChar = '1';
        while (Character.isDigit(firstChar)) {
            firstChar = generateChar();
        }
        StringBuilder out = new StringBuilder();
        out.append(firstChar);
        for (int i = 0; i < random.nextInt(1000); i++) {
            out.append(generateChar());
        }
        withoutWS.append(out);
        return out.toString();
    }

    private char generateChar() {
        return chars.charAt(random.nextInt(chars.length()));
    }
}
