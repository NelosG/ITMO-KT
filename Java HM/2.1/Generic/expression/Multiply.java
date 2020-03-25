package expression;

import expression.exceptions.ArithmeticMyException;
import expression.generic.RIBiD.R;

public class Multiply<T extends Number> extends Binary<T> {

    public Multiply(CommonExpression<T> var1, CommonExpression<T> var2) {
        super(var1, var2);
    }

    @Override
    protected T calc(R<T> r, T a, T b) {
        if (a == null || b == null) return null;
        T res;
        try {
            res = r.mul(a, b);
        } catch (ArithmeticMyException e) {
            return null;
        }
        return res;
    }
}
