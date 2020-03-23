package expression;

public abstract class Unary extends Obj {
    public Unary(Object o) {
        super(o);
    }

    @Override
    public int evaluate(int x) {
        return cal(((CommonExpression) s).evaluate(x));
    }

    @Override
    public double evaluate(double x) {
        return cal(((CommonExpression) s).evaluate(x));
    }

    @Override
    public int evaluate(int x, int y, int z) {
        return cal(((CommonExpression) s).evaluate(x, y, z));
    }
}
