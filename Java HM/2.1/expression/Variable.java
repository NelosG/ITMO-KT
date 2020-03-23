package expression;

public class Variable extends Obj {
    public Variable(String x) {
        super(x);
    }

    @Override
    public int evaluate(int x) {
        return x;
    }

    @Override
    public double evaluate(double x) {
        return x;
    }


    @Override
    public int evaluate(int x, int y, int z) {
        if (((String) s).equals("x")) return x;
        if (((String) s).equals("y")) return y;
        return z;
    }

    @Override
    public String toString() {
        return s.toString();
    }
}
