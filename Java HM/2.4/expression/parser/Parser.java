package expression.parser;

import expression.CommonExpression;
import expression.exceptions.ParserMyException;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface Parser {
    CommonExpression parse(String expression) throws ParserMyException;
}