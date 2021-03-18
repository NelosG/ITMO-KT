package expression.chek;

import expression.CommonExpression;
import expression.Divide;
import expression.exceptions.ArithmeticMyException;

public class CheckedDivide extends Divide {
    public CheckedDivide(CommonExpression var1, CommonExpression var2) {
        super(var1, var2);
        super.ch = "/";
    }


    @Override
    protected int calc(int a, int b) {
        check(a, b);
        return super.calc(a, b);
    }

    private void check(int one, int two) {
        if (one == Integer.MIN_VALUE && two == -1) throw new ArithmeticMyException("OWERFLOW");
        if (two == 0) {
            throw new ArithmeticMyException("DIVISION_BY_ZERO");
        }
    }
}
