package runtime;

public class Pair<T, K> {
    public T first;
    public K second;

    public Pair(T a, K b) {
        first = a;
        second = b;
    }

    @Override
    public boolean equals(Object obj) {
        if (obj != null && obj.getClass() == getClass()) {
            Pair<?, ?> temp = (Pair<?, ?>) obj;
            return first.equals(temp.first) && second.equals(temp.second);
        }
        return false;
    }
}
