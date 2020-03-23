package expression.generic.RIBiD;

import expression.exceptions.ArithmeticMyException;

public class I extends R<Integer> {

    @Override
    public Integer add(Integer a, Integer b) {
        if (b > 0) {
            if (a > Integer.MAX_VALUE - b) throw new ArithmeticMyException("OWERFLOW_EXCEPTION");
        } else {
            if (a < Integer.MIN_VALUE - b) throw new ArithmeticMyException("OWERFLOW_EXCEPTION");
        }
        return a + b;
    }


    @Override
    public Integer sub(Integer a, Integer b) {
        if (b > 0) {
            if (a < Integer.MIN_VALUE + b) throw new ArithmeticMyException("OVERFLOW");
        } else {
            if (a > Integer.MAX_VALUE + b) throw new ArithmeticMyException("OVERFLOW");
        }
        return a - b;
    }

    @Override
    public Integer mul(Integer a, Integer b) {
        if (a > 0 && b > 0 && Integer.MAX_VALUE / a < b) throw new ArithmeticMyException("OWERFLOW");
        if (a > 0 && b < 0 && Integer.MIN_VALUE / a > b) throw new ArithmeticMyException("OWERFLOW");
        if (a < 0 && b > 0 && Integer.MIN_VALUE / b > a) throw new ArithmeticMyException("OWERFLOW");
        if (a < 0 && b < 0 && Integer.MAX_VALUE / a > b) throw new ArithmeticMyException("OWERFLOW");
        return a * b;
    }

    @Override
    public Integer div(Integer a, Integer b) {
        if (a == Integer.MIN_VALUE && b == -1) throw new ArithmeticMyException("OWERFLOW");
        if (b == 0) throw new ArithmeticMyException("DIVISION_BY_ZERO");
        return a / b;
    }


    @Override
    public Integer con(String a) {
        return Integer.valueOf(a);
    }
}
