package expression.parser;

import expression.*;
import expression.chek.CheckedAdd;
import expression.chek.CheckedDivide;
import expression.chek.CheckedNegate;
import expression.chek.CheckedSubtract;
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
                res = new Const(tokenizer.getLastValue());
                currentToken = tokenizer.getNextToken();
                break;
            case VARIABLE:
                res = new Variable(tokenizer.getLastVariable());
                currentToken = tokenizer.getNextToken();
                break;
            case NEGATIVE:
                res = new CheckedNegate(unary());
                break;
            case LOG2:
                res = new Log(unary());
                break;
            case POW2:
                res = new Pow(unary());
                break;
            case OPEN_BRAKE:
                res = addAndSub();
                currentToken = tokenizer.getNextToken();
                break;
            default:
                currentToken = tokenizer.getNextToken();
                res = new Const(0);
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
                    res = new CheckedDivide(res, unary());
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
                    res = new CheckedAdd(res, mulAndDiv());
                    break;
                case SUBTRACT:
                    res = new CheckedSubtract(res, mulAndDiv());
                    break;
                default:
                    return res;
            }
        }
    }

    private CommonExpression shifts() throws ParserMyException {
        CommonExpression res = addAndSub();
        while (true) {
            if (tokenizer.getCurrentToken() == Token.SHIFT_LEFT) {
                res = new LeftShift(res, addAndSub());
            } else if (tokenizer.getCurrentToken() == Token.SHIFT_RIGHT) {
                res = new RightShift(res, addAndSub());
            } else {
                return res;
            }
        }
    }
}
