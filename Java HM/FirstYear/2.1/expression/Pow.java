package expression;

public class Pow extends Unary {
    public Pow(CommonExpression o) {
        super(o);
    }

    @Override
    public int cal(int X) {
        int res = 1;
        if (X < 0 || X > 31) {
            throw new IllegalArgumentException("Only Integer result");
        }
        for (int i = 0; i < X; i++) {
            res *= 2;
        }
        return res;
    }


    @Override
    public String toString() {
        String temp = this.s.toString();
        return "pow2(" + temp + ")";
    }

    @Override
    public int hashCode() {
        return (s).hashCode() + ("pow2").hashCode();
    }
}
