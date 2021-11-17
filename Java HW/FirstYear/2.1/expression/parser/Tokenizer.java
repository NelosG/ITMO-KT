package expression.parser;

import expression.exceptions.ArithmeticMyException;
import expression.exceptions.ParserMyException;

public class Tokenizer {
    private char[] bufChars;
    public int currentIndex;
    public int lastValue;
    public Token currentToken;
    public Token prevToken;
    public String name;
    public String expression;


    public Tokenizer(String string) {
        bufChars = string.toCharArray();
        expression = string;
        currentIndex = 0;
        currentToken = Token.BEGIN;
        prevToken = Token.ERROR;
    }


    public String getExpression() {
        return expression;
    }

    public int getLastValue() {
        return lastValue;
    }

    public String getLastVariable() {
        return name;
    }

    public void skipWhiteSpace() {
        while (currentIndex < expression.length() && Character.isWhitespace(expression.charAt(currentIndex))) {
            currentIndex++;
        }
    }

    public Token getCurrentToken() {
        return currentToken;
    }

    public Token getNextToken() throws ParserMyException {
        prevToken = currentToken;
        nextToken();
        return currentToken;
    }

    public void nextToken() throws ParserMyException {
        skipWhiteSpace();
        if (currentIndex >= expression.length()) {
            if (currentToken != Token.CONST && currentToken != Token.CLOSE_BRAKE && currentToken != Token.VARIABLE) {
                throw new ParserMyException("WRONG_END_EXCEPTION | Expression: " + getExpression());
            } else {
                currentToken = Token.END;
                return;
            }
        }

        char c = bufChars[currentIndex];
        if ((Character.isDigit(c))) {
            NumberToken(c);
        } else if (Character.isLetter((c))) {
            WordToken(c);
        } else {
            if (c == '-' && Character.isDigit(bufChars[currentIndex + 1]) && (currentToken == Token.NEGATIVE || currentToken == Token.BEGIN))
                NumberToken(c);
            else SymbolToken(c);
        }
        currentIndex++;
    }

    private void NumberToken(char c) throws ParserMyException {
        StringBuilder sb = new StringBuilder();
        do {
            sb.append(bufChars[currentIndex]);
            currentIndex++;
        } while ((currentIndex < bufChars.length && Character.isDigit(bufChars[currentIndex])));
        currentIndex--;
        try {
            lastValue = Integer.parseInt(sb.toString());
        } catch (NumberFormatException e) {
            throw new ParserMyException("OVERFLOW");
        }
        currentToken = Token.CONST;
    }

    private void WordToken(char c) throws ParserMyException {
        if (c == 'x' || c == 'y' || c == 'z') {
            currentToken = Token.VARIABLE;
            name = String.valueOf(c);
        } else {
            if ((c == 'l' || c == 'p') && currentIndex < bufChars.length - 4) {
                StringBuilder sb = new StringBuilder();
                sb.append(c);
                sb.append(bufChars[++currentIndex]).append(bufChars[++currentIndex]).append(bufChars[++currentIndex]);
                if (sb.toString().equals("log2")) currentToken = Token.LOG2;
                else if (sb.toString().equals("pow2")) currentToken = Token.POW2;
                else
                    throw new ParserMyException("WRONG_NAME_EXCEPTION: Variable has wrong name | Expression: " + getExpression());
                if (bufChars[currentIndex + 1] == 'x' || bufChars[currentIndex + 1] == 'y' || bufChars[currentIndex + 1] == 'z' || Character.isDigit(bufChars[currentIndex + 1]))
                    throw new ArithmeticMyException("LOG2_POW2_EXCEPTION: Token is incorrect:  " + sb.toString() + bufChars[currentIndex + 1]);
            }
        }

    }

    private void SymbolToken(char c) throws ParserMyException {
        switch (c) {
            case '+':
                if (currentToken == Token.CONST || currentToken == Token.CLOSE_BRAKE || currentToken == Token.VARIABLE) {
                    currentToken = Token.ADD;
                    break;
                } else {
                    throw new ParserMyException("ADD_EXCEPTION: Cant find argument at index " + currentIndex + " | Expression: " + getExpression());
                }
            case '-':
                if (currentToken == Token.CONST || currentToken == Token.VARIABLE || currentToken == Token.CLOSE_BRAKE) {
                    currentToken = Token.SUBTRACT;
                } else {
                    currentToken = Token.NEGATIVE;
                }
                break;
            case '/':
                if (currentToken == Token.CONST || currentToken == Token.CLOSE_BRAKE || currentToken == Token.VARIABLE) {
                    currentToken = Token.DIVIDE;
                    break;
                } else {
                    throw new ParserMyException("DIVIDE_EXCEPTION: Cant find argument at index " + currentIndex + " | Expression: " + getExpression());
                }
            case '*':
                if (currentToken == Token.CONST || currentToken == Token.CLOSE_BRAKE || currentToken == Token.VARIABLE) {
                    currentToken = Token.MULTIPLY;
                    break;
                } else {
                    throw new ParserMyException("MULTIPLY_EXCEPTION: Cant find argument at index " + currentIndex + " | Expression: " + getExpression());
                }
            case '(':
                if (currentToken == Token.ADD || currentToken == Token.DIVIDE || currentToken == Token.MULTIPLY || currentToken == Token.BEGIN ||
                        currentToken == Token.SUBTRACT || currentToken == Token.NEGATIVE || currentToken == Token.OPEN_BRAKE || currentToken == Token.LOG2
                        || currentToken == Token.POW2) {
                    currentToken = Token.OPEN_BRAKE;
                    break;
                } else {
                    throw new ParserMyException("OPEN_BRAKE_EXCEPTION: Wrong Parenthesis at index " + currentIndex + " | Expression: " + getExpression());
                }
            case ')':
                if (currentToken == Token.CONST || currentToken == Token.VARIABLE || currentToken == Token.CLOSE_BRAKE) {
                    currentToken = Token.CLOSE_BRAKE;
                    break;
                } else {
                    throw new ParserMyException("CLOSE_BRAKE_EXCEPTION: Wrong Parenthesis at index " + currentIndex + " | Expression: " + getExpression());
                }
            default:
                throw new ParserMyException("EXCEPTION: UnexpectedCharacter at index " + currentIndex + " | Expression: " + getExpression());
        }
    }
}