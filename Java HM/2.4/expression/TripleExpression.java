package expression;

import expression.generic.RIBiD.R;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface TripleExpression<T extends Number> {
    T evaluate(R<T> r, T x, T y, T z);
}