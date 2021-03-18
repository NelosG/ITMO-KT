package expression;

import expression.generic.RIBiD.R;

public class Variable<T extends Number> extends Obj<T> {
    public Variable(String x) {
        super(x);
    }

    @Override
    public T evaluate(R<T> r, T x, T y, T z) {
        if ((str).equals("x")) return x;
        if ((str).equals("y")) return y;
        return z;
    }

}
