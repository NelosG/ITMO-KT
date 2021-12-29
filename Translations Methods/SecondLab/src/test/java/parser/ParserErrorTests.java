package parser;

import org.hamcrest.core.IsEqual;
import org.junit.Rule;
import org.junit.Test;
import org.junit.rules.ExpectedException;

public class ParserErrorTests {

    @Rule
    public ExpectedException exceptionRule = ExpectedException.none();

    @Test
    public void emptyTest() throws ParserException {
        exceptionRule.expect(parser.ParserException.class);
        exceptionRule.expectMessage("Incorrect Token: expected one of <Type>");
        new Parser().parse("");
    }

    @Test
    public void NoComma1() throws ParserException {
        exceptionRule.expect(parser.ParserException.class);
        exceptionRule.expectMessage("Incorrect Token: expected one of <',' | ';'>");
        new Parser().parse("type var1 var2;");
    }

    @Test
    public void NoComma2() throws ParserException {
        exceptionRule.expect(parser.ParserException.class);
        exceptionRule.expectMessage("Incorrect Token: expected one of <',' | ';'>");
        new Parser().parse("type var1 *var2;");
    }

    @Test
    public void NoComma3() throws ParserException {
        exceptionRule.expect(parser.ParserException.class);
        exceptionRule.expectMessage("Incorrect Token: expected one of <',' | ';'>");
        new Parser().parse("type *var1 var2;");
    }

    @Test
    public void NoComma4() throws ParserException {
        exceptionRule.expect(parser.ParserException.class);
        exceptionRule.expectMessage("Incorrect Token: expected one of <',' | ';'>");
        new Parser().parse("type *var1 *var2;");
    }

    @Test
    public void NoVariable1() throws ParserException {
        exceptionRule.expect(parser.ParserException.class);
        exceptionRule.expectMessage("Incorrect Token: expected one of <'*', Variable>");
        new Parser().parse("type;");
    }

    @Test
    public void NoVariable2() throws ParserException {
        exceptionRule.expect(parser.ParserException.class);
        exceptionRule.expectMessage("Incorrect Token: expected one of <'*', Variable>");
        new Parser().parse("type,var2;");
    }

    @Test
    public void NoVariable3() throws ParserException {
        exceptionRule.expect(parser.ParserException.class);
        exceptionRule.expectMessage("Incorrect Token: expected one of <'*', Variable>");
        new Parser().parse("type***;");
    }

    @Test
    public void NoVariable4() throws ParserException {
        exceptionRule.expect(parser.ParserException.class);
        exceptionRule.expectMessage("Incorrect Token: expected one of <'*', Variable>");
        new Parser().parse("type***,var2;");
    }

    @Test
    public void NoVariable5() throws ParserException {
        exceptionRule.expect(parser.ParserException.class);
        exceptionRule.expectMessage("Incorrect Token: expected one of <'*', Variable>");
        new Parser().parse("type,**var2;");
    }

    @Test
    public void NoVariable6() throws ParserException {
        exceptionRule.expect(parser.ParserException.class);
        exceptionRule.expectMessage("Incorrect Token: expected one of <'*', Variable>");
        new Parser().parse("type***,**var2;");
    }

    @Test
    public void wrongTypeName() throws ParserException {
        String wrongSymnols = "~1`'\"'!@#№%^&*()-><?\\|/";
        String suffix = "_avc";
        for (char c : wrongSymnols.toCharArray()) {
            TypeCheck(c + suffix);
            VariableCheck(c + suffix);
        }
    }


    private void TypeCheck(String typeName) throws ParserException {
        exceptionRule.expect(parser.ParserException.class);
        Throwable cause = new tokenizer.TokenizerException ("Unexpected token: " + typeName);
        exceptionRule.expectCause(IsEqual.equalTo(cause));
        exceptionRule.expectMessage("Can't read first token");
        new Parser().parse(typeName + " variable;");
    }

    private void VariableCheck(String variableName) throws ParserException {
        exceptionRule.expect(parser.ParserException.class);
        Throwable cause = new tokenizer.TokenizerException ("Unexpected token: " + variableName);
        exceptionRule.expectCause(IsEqual.equalTo(cause));
        new Parser().parse("type " + variableName + ";");
    }
}
