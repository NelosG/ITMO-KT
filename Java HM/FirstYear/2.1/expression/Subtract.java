package expression;


public class Subtract extends Binary {

    public Subtract(CommonExpression var1, CommonExpression var2) {
        super(var1, var2);
        super.ch = "-";
    }

    @Override
    protected int calc(int a, int b) {
        return a - b;
    }

    @Override
    protected double calc(double a, double b) {
        return a - b;
    }

}
