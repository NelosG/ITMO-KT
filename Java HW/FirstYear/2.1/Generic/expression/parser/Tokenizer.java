package expression.parser;

import expression.exceptions.ParserMyException;

public class Tokenizer {
    public int currentIndex;
    public String lastConst = "";
    public Token currentToken;
    public Token prevToken;
    public String name;
    public String expression;
    private char[] bufChars;


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

    public String getLastConst() {
        return lastConst;
    }

    public String getLastVariable() {
        return name;
    }

    public void skipWhiteSpace() {
        while (currentIndex < expression.length() && Character.isWhitespace(expression.charAt(currentIndex))) {
            currentIndex++;
        }
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
                currentToken = Token.END;
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
        lastConst = sb.toString();
        currentIndex--;
        currentToken = Token.CONST;
    }

    private void WordToken(char c) throws ParserMyException {
        if (c == 'x' || c == 'y' || c == 'z') {
            currentToken = Token.VARIABLE;
            name = String.valueOf(c);
        } else {
            throw new ParserMyException("Illegal Var");
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
                        currentToken == Token.SUBTRACT || currentToken == Token.NEGATIVE || currentToken == Token.OPEN_BRAKE) {
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