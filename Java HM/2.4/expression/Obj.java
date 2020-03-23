package expression;

import expression.generic.RIBiD.R;

public abstract class Obj<T extends Number> implements CommonExpression<T> {

    public String str;

    public Obj(String o) {
        this.str = o;
    }

    @Override
    public T evaluate(R<T> r, T x, T y, T z) {
        return cal(r, x);
    }

    public T cal(R<T> r, T x) {
        return r.con(str);
    }
}
