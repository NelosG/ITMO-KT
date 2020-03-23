package expression;

public class Const extends Obj {
    boolean p;

    public Const(Object c) {
        super(c);
        if (c.getClass() == Double.class) {
            p = true;
        } else {
            p = false;
        }
    }

    @Override
    public String toString() {
        if (p) return ((Double) s).toString();
        return ((Integer) (int) (double) s).toString();
    }
}
