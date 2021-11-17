package expression.chek;


import expression.CommonExpression;
import expression.UnaryMinus;
import expression.exceptions.ArithmeticMyException;

public class CheckedNegate extends UnaryMinus {
    public CheckedNegate(CommonExpression o) {
        super(o);
    }

    @Override
    public int cal(int x) {
        check(x);
        return -x;
    }

    public void check(int x) {
        if (x == Integer.MIN_VALUE) {
            throw new ArithmeticMyException("OWERFLOW");
        }
    }
}
