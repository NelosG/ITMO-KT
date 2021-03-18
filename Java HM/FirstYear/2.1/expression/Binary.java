package expression;


public abstract class Binary implements CommonExpression {

    public CommonExpression var1, var2;
    public String ch;

    public Binary(CommonExpression var1, CommonExpression var2) {
        this.var1 = var1;
        this.var2 = var2;
    }


    protected abstract int calc(int a, int b);
    protected abstract double calc(double a, double b);

    @Override
    public int evaluate(int x, int y, int z) {
        return calc(var1.evaluate(x, y, z), var2.evaluate(x, y, z));
    }

    @Override
    public int evaluate(int x) {
        return calc(var1.evaluate(x), var2.evaluate(x));
    }

    @Override
    public double evaluate(double x) {
        return var1.evaluate(x) + var2.evaluate(x);
    }
    @Override
    public String toString() {
        return ("(" + var1.toString() + " " + ch + " " + var2.toString() + ")");
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null || obj.getClass() != getClass()) return false;
        return var1.equals(((Binary) obj).var1) && var2.equals(((Binary) obj).var2);
    }


    @Override
    public int hashCode() {
        return var1.hashCode() * 17 + ch.hashCode() * 19 + var2.hashCode() * 31;
    }

}

