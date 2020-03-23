package expression;

public abstract class Obj implements CommonExpression {

    public Object s;

    public Obj(Object o) {

        if (o.getClass() == Integer.class) {
            this.s = (double) (int) o;
        } else {
            if (o.getClass() == Double.class) {
                this.s = (double) o;
            } else this.s = o;
        }
    }

    @Override
    public int evaluate(int x) {
        return cal((int) evaluate((double) x));
    }

    @Override
    public double evaluate(double x) {
        return cal((double) s);
    }


    @Override
    public int evaluate(int x, int y, int z) {
        return cal(evaluate(x));
    }

    public int cal(int x) {
        return x;
    }

    public double cal(double x) {
        return x;
    }

    @Override
    public String toString() {
        return ((Integer) (int) (double) s).toString();
    }

    @Override
    public boolean equals(Object obj) {
        if (obj == null || obj.getClass() != getClass()) return false;
        return s.equals(((Obj) obj).s);
    }

    @Override
    public int hashCode() {
        return (s).hashCode();
    }
}
