package expression;

import expression.exceptions.ArithmeticMyException;

public class RightShift extends Binary {
    public RightShift(CommonExpression var1, CommonExpression var2) {
        super(var1, var2);
        super.ch = ">>";
    }

    @Override
    protected int calc(int a, int b) {
        return a >> b;
    }

    @Override
    protected double calc(double a, double b) {
        throw new ArithmeticMyException("Only Integer Shifts");
    }
}
