package expression;

import expression.exceptions.ArithmeticMyException;

public class Log extends Unary {
    public Log(CommonExpression o) {
        super(o);
    }



    @Override
    public int cal(int X) {
        int res = 0;
        if (X <= 0) throw new ArithmeticMyException("Const under Log must be >= 0");
        if (X == 1) return 0;
        while (true) {
            X /= 2;
            res++;
            if (X == 1) {
                return res;
            }
        }
    }



    @Override
    public String toString() {
        String temp = this.s.toString();
        return "log2(" + temp + ")";
    }

    @Override
    public int hashCode() {
        return (s).hashCode() + ("log2").hashCode();
    }
}
