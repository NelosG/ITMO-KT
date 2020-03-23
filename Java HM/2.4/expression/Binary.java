package expression;


import expression.generic.RIBiD.R;

public abstract class Binary<T extends Number> implements CommonExpression<T> {
    public CommonExpression<T> var1, var2;

    public Binary(CommonExpression<T> var1, CommonExpression<T> var2) {
        this.var1 = var1;
        this.var2 = var2;
    }


    @Override
    public T evaluate(R<T> r, T x, T y, T z) {
        return calc(r, var1.evaluate(r, x, y, z), var2.evaluate(r, x, y, z));
    }

    protected abstract T calc(R<T> r, T evaluate, T evaluate1);
}

