package expression.chek;

import expression.Add;
import expression.CommonExpression;
import expression.exceptions.ArithmeticMyException;

public class CheckedAdd extends Add {

    public CheckedAdd(CommonExpression var1, CommonExpression var2) {
        super(var1, var2);
        super.ch = "+";
    }

    @Override
    protected int calc(int a, int b) {
        check(a, b);
        return super.calc(a, b);
    }

    private void check(int one, int two) throws ArithmeticMyException {
        if (two > 0) {
            if (one > Integer.MAX_VALUE - two) {
                throw new ArithmeticMyException("OWERFLOW_EXCEPTION");
            }
        } else {
            if (one < Integer.MIN_VALUE - two) {
                throw new ArithmeticMyException("OWERFLOW_EXCEPTION");
            }
        }
    }
}