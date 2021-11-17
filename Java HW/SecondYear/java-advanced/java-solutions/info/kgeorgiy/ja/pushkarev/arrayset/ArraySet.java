package info.kgeorgiy.ja.pushkarev.arrayset;

import java.util.*;

public class ArraySet<E> extends AbstractSet<E> implements NavigableSet<E> {
    private final List<E> elements;
    private final Comparator<? super E> comparator;

    // :NOTE: Дублирование
    public ArraySet() {
        this.elements = List.of();
        this.comparator = null;
    }

    public ArraySet(final Comparator<? super E> comparator) {
        this.elements = List.of();
        this.comparator = comparator;
    }

    public ArraySet(final SortedSet<E> s) {
        this.elements = List.copyOf(s);
        this.comparator = s.comparator();
    }

    public ArraySet(final Collection<? extends E> elements) {
        this(elements, null);
    }

    public ArraySet(final Collection<? extends E> elements, final Comparator<? super E> comparator) {
        final Set<E> set = new TreeSet<>(comparator);
        set.addAll(elements);
        this.elements = List.copyOf(set);
        this.comparator = comparator;
    }

    private ArraySet(final List<E> elements, final Comparator<? super E> comparator) {
        this.elements = elements;
        this.comparator = comparator;
    }

    @SuppressWarnings("unchecked")
    @Override
    public boolean contains(final Object o) {
        return Collections.binarySearch(elements, (E) o, comparator) >= 0;
    }

    @Override
    public E floor(final E e) {
        return elementAt(getIndex(e, false, true));
    }

    @Override
    public E ceiling(final E e) {
        return elementAt(getIndex(e, true, true));
    }

    @Override
    public E lower(final E e) {
        return elementAt(getIndex(e, false, false));
    }

    @Override
    public E higher(final E e) {
        return elementAt(getIndex(e, true, false));
    }

    @Override
    public E pollFirst() {
        throw new UnsupportedOperationException();
    }

    @Override
    public E pollLast() {
        throw new UnsupportedOperationException();
    }

    // :NOTE: Многократный вызов unmodifiableList
    @Override
    public Iterator<E> iterator() {
        return Collections.unmodifiableList(elements).iterator();
    }

    @Override
    public Iterator<E> descendingIterator() {
        return descendingSet().iterator();
    }

    @Override
    public NavigableSet<E> descendingSet() {
        return new ArraySet<>(new DescendingList<>(elements), Collections.reverseOrder(comparator));
    }

    @Override
    public NavigableSet<E> subSet(final E fromElement, final boolean fromInclusive, final E toElement, final boolean toInclusive) {
        // :NOTE: Явное приведение типа
        @SuppressWarnings("unchecked") final int comp = comparator == null ?
                ((Comparable<E>) fromElement).compareTo(toElement)
                : comparator.compare(fromElement, toElement);
        if (comp > 0) {
            throw new IllegalArgumentException("fromElement > toElement");
        }

        return subSetImpl(fromElement, fromInclusive, toElement, toInclusive);
    }

    @Override
    public NavigableSet<E> headSet(final E toElement, final boolean inclusive) {
        return isEmpty() ? this :
                subSetImpl(first(), true, toElement, inclusive);
    }

    @Override
    public NavigableSet<E> tailSet(final E fromElement, final boolean inclusive) {
        return isEmpty() ? this :
                subSetImpl(fromElement, inclusive, last(), true);
    }

    @Override
    public Comparator<? super E> comparator() {
        return comparator;
    }

    @Override
    public SortedSet<E> subSet(final E fromElement, final E toElement) {
        return subSet(fromElement, true, toElement, false);
    }

    @Override
    public SortedSet<E> headSet(final E toElement) {
        return headSet(toElement, false);
    }

    @Override
    public SortedSet<E> tailSet(final E fromElement) {
        return tailSet(fromElement, true);
    }

    // :NOTE: Неаккуратно
    @Override
    public E first() {
        final E e = elementAt(0);
        if (e == null) {
            throw new NoSuchElementException();
        }
        return e;
    }

    @Override
    public E last() {
        final E e = elementAt(size() - 1);
        if (e == null) {
            throw new NoSuchElementException();
        }
        return e;
    }

    @Override
    public int size() {
        return elements.size();
    }

    private NavigableSet<E> subSetImpl(final E fromElement, final boolean fromInclusive, final E toElement, final boolean toInclusive) {
        final int fromIndex = getIndex(fromElement, true, fromInclusive);
        final int toIndex = getIndex(toElement, false, toInclusive);
        if (fromIndex > toIndex)
            return new ArraySet<>(comparator);
        return new ArraySet<>(elements.subList(fromIndex, toIndex + 1), comparator);
    }

    private int getIndex(final E e, final boolean rightSide, final boolean inclusive) {
        int index = Collections.binarySearch(elements, e, comparator);
        if (index < 0) {
            index = -index - (rightSide ? 1 : 2);
        } else if (!inclusive) {
            index += (rightSide ? 1 : -1);
        }
        return index;
    }

    private E elementAt(final int index) {
        return index >= 0 && index < elements.size() ? elements.get(index) : null;
    }

    private static class DescendingList<E> extends AbstractList<E> {

        private final List<E> elements;

        private final boolean reversed;

        DescendingList(final List<E> elements) {
            if (elements.getClass() == DescendingList.class) {
                final DescendingList<E> dl = (DescendingList<E>) elements;
                this.elements = dl.elements;
                this.reversed = !dl.reversed;
            } else {
                this.elements = elements;
                this.reversed = true;
            }
        }

        @Override
        public E get(final int index) {
            if (reversed)
                return elements.get(size() - 1 - index);
            return elements.get(index);
        }

        @Override
        public int size() {
            return elements.size();
        }
    }
}
