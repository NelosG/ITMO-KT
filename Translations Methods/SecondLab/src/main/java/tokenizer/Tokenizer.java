package tokenizer;

import java.util.StringTokenizer;
import java.util.regex.Pattern;

import static tokenizer.Token.*;

public class Tokenizer {
    private final StringTokenizer stringTokenizer;
    private boolean isType = true;
    private final Pattern pattern = Pattern.compile("[_$a-zA-Z][\\w$]*");
    private final Pattern patternNumb = Pattern.compile("[0-9]+");

    private Token token;
    private String tokenValue;

    public Tokenizer(String text) {

        stringTokenizer = new StringTokenizer(text, "\n\r\t *,;[]", true);
    }

    public boolean hasMoreTokens() {
        return stringTokenizer.hasMoreTokens();
    }

    public Token token() {
        return token;
    }

    public String tokenValue() {
        return tokenValue;
    }

    public Token nextToken() throws TokenizerException {
        if(!stringTokenizer.hasMoreTokens()) {
            token = END;
            return END;
        }
        tokenValue = stringTokenizer.nextToken();
        if (patternNumb.asMatchPredicate().test(tokenValue)) {
            token = NUMB;
            return token;
        }
        if (pattern.asMatchPredicate().test(tokenValue)) {
            return nameOrType();
        }
        Token res;
        switch (tokenValue) {
            case " ", "\n", "\r", "\t" -> {
                return nextToken();
            }
            case "," -> res = COMMA;
            case "*" -> res = STAR;
            case "[" -> res = LPAR;
            case "]" -> res = RPAR;
            case ";" -> {
                isType = true;
                res = SEMICOLON;
            }
            default -> throw new TokenizerException("Unexpected token: " + tokenValue);
        }
        token = res;
        return res;
    }

    private Token nameOrType() {
        token = isType ? TYPE : VARIABLE;
        isType = false;
        return token;
    }
}
