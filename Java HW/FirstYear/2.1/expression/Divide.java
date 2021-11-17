package expression;


public class Divide extends Binary {
    public Divide(CommonExpression var1, CommonExpression var2) {
        super(var1, var2);
        super.ch = "/";
    }

    @Override
    protected int calc(int a, int b) {
        return a / b;
    }

    @Override
    protected double calc(double a, double b) {
        return a / b;
    }


}

