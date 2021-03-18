package expression.chek;


import expression.CommonExpression;
import expression.Multiply;
import expression.exceptions.ArithmeticMyException;

public class CheckedMultiply extends Multiply {

    public CheckedMultiply(CommonExpression var1, CommonExpression var2) {
        super(var1, var2);
        super.ch = "*";
    }

    @Override
    public int calc(int a, int b) {
        check(a, b);
        return a * b;
    }

    private void check(int one, int two) {
        if (one > 0 && two > 0 && Integer.MAX_VALUE / one < two) throw new ArithmeticMyException("OWERFLOW");
        if (one > 0 && two < 0 && Integer.MIN_VALUE / one > two) throw new ArithmeticMyException("OWERFLOW");
        if (one < 0 && two > 0 && Integer.MIN_VALUE / two > one) throw new ArithmeticMyException("OWERFLOW");
        if (one < 0 && two < 0 && Integer.MAX_VALUE / one > two) throw new ArithmeticMyException("OWERFLOW");

    }
}
