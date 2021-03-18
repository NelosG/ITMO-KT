package expression.generic.RIBiD;

public abstract class R<T extends Number> {
    public abstract T add(T a, T b);

    public abstract T sub(T a, T b);

    public abstract T mul(T a, T b);

    public abstract T div(T a, T b);

    public abstract T con(String str);
}
