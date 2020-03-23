package expression;

public class Multiply extends Binary {

    public Multiply(CommonExpression var1, CommonExpression var2) {
        super(var1, var2);
        super.ch = "*";
    }

    @Override
    public int calc(int a, int b) {
        return a * b;
    }

    @Override
    public double calc(double a, double b) {
        return a * b;
    }

}
