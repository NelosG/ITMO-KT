package info.kgeorgiy.java.advanced.arrayset;

import java.util.Comparator;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class NamedComparator implements Comparator<Integer> {
    private final String name;
    private final Comparator<Integer> comparator;

    NamedComparator(final String name, final Comparator<Integer> comparator) {
        this.name = name;
        this.comparator = comparator;
    }

    @Override
    public int compare(final Integer o1, final Integer o2) {
        return comparator.compare(o1, o2);
    }

    @Override
    public String toString() {
        return name;
    }
}
