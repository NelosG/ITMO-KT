package tokenizer;


import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class TokenizerTests {

    private String readOutput(Tokenizer tokenizer) throws TokenizerException {
        StringBuilder result = new StringBuilder();
        while (tokenizer.hasMoreTokens()) {
            String res = "";
            switch (tokenizer.nextToken()) {
                case STAR -> res = "*";
                case COMMA -> res = ",";
                case SEMICOLON -> res = ";";
                case TYPE -> res = "Type";
                case VARIABLE -> res = "Var";
                case END -> {
                    return result.toString();
                }
            }
            result.append(res);
        }
        throw new TokenizerException("Hasn't End");
    }

    @Test
    public void baseTest() throws TokenizerException {
        String text = """
                int * a,
                *b,
                * * * * * * cd;
                """;
        assertEquals("Type*Var,*Var,******Var;", readOutput(new Tokenizer(text)));
    }

    @Test
    public void NamesOfTypesAndVariables1() throws TokenizerException {
        String text = """
                __aWfUl_TyPe6_6_6a__ cd;
                """;
        assertEquals("TypeVar;", readOutput(new Tokenizer(text)));
    }

    @Test
    public void NamesOfTypesAndVariables2() throws TokenizerException {
        String text = """
                int __1A$vds$as45$5$ref$43dfd_$43_$ada_$433$_4343$_rerege$_reve$_433$_$___;
                """;
        assertEquals("TypeVar;", readOutput(new Tokenizer(text)));
    }

    @Test
    public void NamesOfTypesAndVariables3() throws TokenizerException {
        String text = """
                __1A$vds$as45$5$ref$43dfd_$43_$ada_$433$_4343$_rerege$_reve$_433$_$___
                 __1A$vds$as45$5$ref$43dfd_$43_$ada_$433$_4343$_rerege$_reve$_433$_$___;
                """;
        assertEquals("TypeVar;", readOutput(new Tokenizer(text)));
    }
}
