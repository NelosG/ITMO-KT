package expression;

public class UnaryMinus extends Obj {
    public UnaryMinus(CommonExpression o) {
        super(o);
    }

    @Override
    public int cal(int x) {

        return -x;
    }
    @Override
    public int evaluate(int x) {
        return cal(((CommonExpression) s).evaluate(x));
    }
    @Override
    public int evaluate(int x, int y, int z) {
        return cal(((CommonExpression) s).evaluate(x, y, z));
    }

    @Override
    public String toString() {
        String temp = this.s.toString();
        return ("-" + temp);
    }

    @Override
    public int hashCode() {
        return ((Double) ((double) s * -1)).hashCode();
    }
}
