package expression.parser;

import expression.*;
import expression.exceptions.ParserMyException;


public class ExpressionParser implements Parser {

    private Tokenizer tokenizer;
    private Token currentToken = Token.BEGIN;

    public CommonExpression parse(String expression) throws ParserMyException {
        tokenizer = new Tokenizer(expression);
        currentToken = Token.BEGIN;
        CommonExpression res = addAndSub();
        expect(Token.END);
        return res;
    }

    private void expect(Token token) throws ParserMyException {
        if (tokenizer.currentIndex <= tokenizer.expression.length() && token != currentToken) {
            if (currentToken == Token.END) {
                throw new ParserMyException("CLOSE_BRAKE_ERROR: MissingParenthesis at index " + tokenizer.currentIndex + " | Expression: " + tokenizer.getExpression());
            } else {
                throw new ParserMyException("ERROR: UnexpectedCharacter at index " + tokenizer.currentIndex + " | Expression: " + tokenizer.getExpression());
            }
        }
    }

    private CommonExpression unary() throws ParserMyException {
        CommonExpression res;
        currentToken = tokenizer.getNextToken();
        switch (currentToken) {
            case CONST:
                res = new Const(tokenizer.getLastConst());
                currentToken = tokenizer.getNextToken();
                break;
            case VARIABLE:
                res = new Variable(tokenizer.getLastVariable());
                currentToken = tokenizer.getNextToken();
                break;
            case OPEN_BRAKE:
                res = addAndSub();
                currentToken = tokenizer.getNextToken();
                break;
            default:
                currentToken = tokenizer.getNextToken();
                res = new Const("0");
                break;
        }
        return res;
    }

    private CommonExpression mulAndDiv() throws ParserMyException {
        CommonExpression res = unary();
        while (true) {
            switch (currentToken) {
                case MULTIPLY:
                    res = new Multiply(res, unary());
                    break;
                case DIVIDE:
                    res = new Divide<>(res, unary());
                    break;
                default:
                    return res;
            }
        }
    }

    private CommonExpression addAndSub() throws ParserMyException {
        CommonExpression res = mulAndDiv();
        while (true) {
            switch (currentToken) {
                case ADD:
                    res = new Add(res, mulAndDiv());
                    break;
                case SUBTRACT:
                    res = new Subtract(res, mulAndDiv());
                    break;
                default:
                    return res;
            }
        }
    }

}
