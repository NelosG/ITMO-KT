package calc;

import org.junit.Test;
import runtime.Pair;

import java.util.*;

import calc.CalcLexer.CalcToken;

public class TestCalcLexer {

    private final List<Pair<CalcToken, String>> operation = List.of(
            new Pair<>(CalcToken.DIV, "/"),
            new Pair<>(CalcToken.MUL, "*"),
            new Pair<>(CalcToken.PLUS, "+"),
            new Pair<>(CalcToken.MINUS, "-"),
            new Pair<>(CalcToken.LPAREN, "("),
            new Pair<>(CalcToken.RPAREN, ")")
    );
    private final Random random = new Random();
    private final String WS = " \n\r\t";

    @Test
    public void BaseTest() {
        List<Pair<CalcToken, String>> list = new ArrayList<>();
        list.add(new Pair<>(CalcToken.NUMBER, "1"));
        list.add(operation.get(0));
        list.add(new Pair<>(CalcToken.NUMBER, "5"));

        testList(list);
    }

    @Test
    public void BaseDoubleTest() {
        List<Pair<CalcToken, String>> list = new ArrayList<>();
        list.add(new Pair<>(CalcToken.NUMBER, "1"));
        list.add(new Pair<>(CalcToken.POINT, "."));
        list.add(new Pair<>(CalcToken.NUMBER, "11"));

        list.add(operation.get(0));
        list.add(new Pair<>(CalcToken.NUMBER, "5"));
        list.add(new Pair<>(CalcToken.POINT, "."));
        list.add(new Pair<>(CalcToken.NUMBER, "23"));


        testList(list);
    }


    @Test
    public void RandomTest() {
        List<Pair<CalcToken, String>> list = new ArrayList<>();
        list.add(new Pair<>(CalcToken.NUMBER, "1"));
        list.add(operation.get(0));
        list.add(new Pair<>(CalcToken.NUMBER, "5"));
        for (int i = 0; i < 1000; i++) {
            if (random.nextInt(2) == 0) {
                list = wrap(list);
            } else {
                list.add(operation.get(random.nextInt(4)));
                list.addAll(generateNumber());
            }
        }

        testList(list);
    }

    private List<Pair<CalcToken, String>> generateNumber() {
        if (random.nextInt(2) == 0) {
            return List.of(new Pair<>(CalcToken.NUMBER, Integer.toString(random.nextInt(Integer.MAX_VALUE))));
        }
        return List.of(
                new Pair<>(CalcToken.NUMBER, Integer.toString(random.nextInt(Integer.MAX_VALUE))),
                new Pair<>(CalcToken.POINT, "."),
                new Pair<>(CalcToken.NUMBER, Integer.toString(random.nextInt(Integer.MAX_VALUE)))
        );
    }

    private List<Pair<CalcToken, String>> wrap(List<Pair<CalcToken, String>> list) {
        List<Pair<CalcToken, String>> temp = list;
        list = new ArrayList<>();
        list.add(operation.get(4));
        list.addAll(temp);
        list.add(operation.get(5));
        return list;
    }


    private void testList(List<Pair<CalcToken, String>> list) {
        StringBuilder sb = new StringBuilder();
        for (var p : list) {
            addWS(sb.append(p.second));
        }
        CalcLexer calcLexer = new CalcLexer(sb.toString());
        for (var p : list) {
            if (calcLexer.getToken() != CalcToken.EOF) {
                if (!p.equals(new Pair<>(calcLexer.getToken(), calcLexer.getTokenValue()))) {
                    throw new IllegalArgumentException(p + " != " + calcLexer.getToken() + " : " + calcLexer.getTokenValue());
                }
            } else {
                throw new IllegalStateException("Find EOF");
            }
            calcLexer.nextToken();
        }
        if (calcLexer.getToken() != CalcToken.EOF) {
            throw new IllegalStateException("Don't find EOF");
        }
    }

    private void addWS(StringBuilder sb) {
        int size = random.nextInt(4);
        for (int i = 0; i < size; i++) {
            sb.append(WS.charAt(random.nextInt(WS.length())));
        }
    }
}
