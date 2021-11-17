package expression.parser;

import expression.*;
import expression.exceptions.*;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface Parser {
    CommonExpression parse(String expression) throws ParserMyException;
}