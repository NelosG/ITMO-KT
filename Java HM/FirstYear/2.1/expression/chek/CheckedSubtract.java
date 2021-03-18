package expression.chek;


import expression.CommonExpression;
import expression.Subtract;
import expression.exceptions.ArithmeticMyException;

public class CheckedSubtract extends Subtract {
    public CheckedSubtract(CommonExpression var1, CommonExpression var2) {
        super(var1, var2);
        super.ch = "-";
    }

    public int calc(int one, int two) {
        check(one, two);
        return one - two;
    }

    private void check(int one, int two) throws ArithmeticMyException {
        if (two > 0) {
            if (one < Integer.MIN_VALUE + two) {
                throw new ArithmeticMyException("OVERFLOW");
            }
        } else {
            if (one > Integer.MAX_VALUE + two) {
                throw new ArithmeticMyException("OVERFLOW");
            }
        }
    }
}
