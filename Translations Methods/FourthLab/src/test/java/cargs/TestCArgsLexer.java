package cargs;

import org.junit.Test;

import static org.junit.Assert.assertEquals;

import cargs.CArgsLexer;

public class TestCArgsLexer {
    private String readOutput(CArgsLexer lexer) {
        StringBuilder result = new StringBuilder();
        while (true) {

            String res = "";
            switch (lexer.getToken()) {
                case STAR -> res = "*";
                case COMMA -> res = ",";
                case SEMICOLON -> res = ";";
                case NAME -> res = lexer.getTokenValue();
                case EOF -> {
                    return result.toString();
                }
            }
            lexer.nextToken();
            result.append(res);
        }
    }

    @Test
    public void baseTest() {
        String text = """
                int * a,
                *b,
                * * * * * * cd;
                """;
        assertEquals("int*a,*b,******cd;", readOutput(new CArgsLexer(text)));
    }

    @Test
    public void NamesOfTypesAndVariables1() {
        String text = """
                __aWfUl_TyPe6_6_6a__ cd;
                """;
        assertEquals("__aWfUl_TyPe6_6_6a__cd;", readOutput(new CArgsLexer(text)));
    }

    @Test
    public void NamesOfTypesAndVariables2() {
        String text = """
                int __1A$vds$as45$5$ref$43dfd_$43_$ada_$433$_4343$_rerege$_reve$_433$_$___;
                """;
        assertEquals("int__1A$vds$as45$5$ref$43dfd_$43_$ada_$433$_4343$_rerege$_reve$_433$_$___;", readOutput(new CArgsLexer(text)));
    }

    @Test
    public void NamesOfTypesAndVariables3() {
        String text = """
                __1A$vds$as45$5$ref$43dfd_$43_$ada_$433$_4343$_rerege$_reve$_433$_$___
                 __1A$vds$as45$5$ref$43dfd_$43_$ada_$433$_4343$_rerege$_reve$_433$_$___;
                """;
        assertEquals("__1A$vds$as45$5$ref$43dfd_$43_$ada_$433$_4343$_rerege$_reve$_433$_$___" +
                "__1A$vds$as45$5$ref$43dfd_$43_$ada_$433$_4343$_rerege$_reve$_433$_$___;", readOutput(new CArgsLexer(text)));
    }
}
