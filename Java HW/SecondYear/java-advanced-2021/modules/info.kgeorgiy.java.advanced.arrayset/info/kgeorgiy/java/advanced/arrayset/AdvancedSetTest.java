package info.kgeorgiy.java.advanced.arrayset;

import org.junit.*;
import org.junit.runners.MethodSorters;

import java.util.*;
import java.util.stream.Stream;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class AdvancedSetTest extends NavigableSetTest {
    @Test
    public void test06_immutable() {
        final SortedSet<Integer> set = set(List.of(1));
        checkUnsupported("add", () -> set.add(1));
        checkUnsupported("addAll", () -> set.addAll(List.of(1)));
        checkUnsupported("clear", set::clear);
        checkUnsupported("iterator.remove", () -> {
            final Iterator<Integer> iterator = set.iterator();
            iterator.next();
            iterator.remove();
        });
        checkUnsupported("remove", () -> set.remove(1));
        checkUnsupported("removeAll", () -> set.removeAll(List.of(1)));
        checkUnsupported("retainAll", () -> set.retainAll(List.of(0)));
    }

    private static void checkUnsupported(final String method, final Runnable command) {
        try {
            command.run();
            Assert.fail("Method '" + method + "' should throw UnsupportedOperationException");
        } catch (final UnsupportedOperationException ignore) {
        }
    }

    @Test
    public void test10_containsAllPerformance() {
        performance("contains", () -> {
            final SortedSet<Integer> set = performanceSet();
            Assert.assertTrue(null, set.containsAll(new ArrayList<>(set)));
        });
    }

    @Test
    public void test15_tailSetPerformance() {
        performance("tailSet", () -> {
            final SortedSet<Integer> set = performanceSet();
            for (final Integer element : set) {
                Assert.assertTrue(set.tailSet(element).contains(element));
            }
        });
    }

    @Test
    public void test18_copySource() {
        final List<Integer> data = List.of(1, 10, 100);
        final List<Integer> list = new ArrayList<>(data);
        final TreeSet<Integer> set = new TreeSet<>(data);
        final SortedSet<Integer> integers = create(new Object[]{list}, Collection.class);
        assertEq(integers, set, "initial");
        list.set(1, 20);
        assertEq(integers, set, "mutated");
    }

    @Test
    public void test19_immutableSource() {
        final List<Integer> data = List.of(1, 100, 10);
        final SortedSet<Integer> integers = create(new Object[]{data}, Collection.class);
        assertEq(integers, new TreeSet<>(List.of(1, 10, 100)), "initial");
    }

    @Test
    public void test38_mutators() {
        final NavigableSet<Integer> set = set(List.of(1, 2, 3), Integer::compareTo);
        checkUnsupported("pollFirst", set::pollFirst);
        checkUnsupported("pollLast", set::pollLast);
    }

    protected static <E extends Exception> void assertThrows(final Class<E> type, final Runnable mutator) {
        try {
            mutator.run();
            Assert.fail("Expected " + type.getSimpleName());
        } catch (final Exception e) {
            if (!type.isInstance(e)) {
                throw e;
            }
        }
    }

    @Test @Override
    public void test39_descendingSet() {
        super.test39_descendingSet();
        testDescendingSet(treeSet(TEST_DESCENDING_SET_DATA, null), set(TEST_DESCENDING_SET_DATA, null));
    }

    @Test
    public void test40_descendingSetPerformance() {
        testDescendingSetPerformance(10, Integer::compareUnsigned, 300);
        testDescendingSetPerformance(10, null, 300);
        testDescendingSetPerformance(PERFORMANCE_SIZE / 250, Integer::compare, 2);
        testDescendingSetPerformance(PERFORMANCE_SIZE / 250, null, 2);
    }

    private static void testDescendingSetPerformance(final int size, final Comparator<Integer> comparator, final int iterations) {
        final Collection<Integer> data = performanceSet(size);
        final NavigableSet<Integer> treeSet = treeSet(data, comparator);
        final NavigableSet<Integer> set = set(data, comparator);
        performance(
                "descendingSet",
                () -> Stream.iterate(set, NavigableSet::descendingSet).skip(PERFORMANCE_SIZE & -2).findFirst().ifPresent(deepSet -> {
                    for (int i = 0; i < iterations; i++) {
                        testDescendingSet(treeSet, deepSet);
                    }
                })
        );
    }

    @Override
    protected boolean compare(final Comparator<Integer> comparator, final Integer from, final Integer to) {
        return comparator == null ? from <= to : comparator.compare(from, to) <= 0;
    }

    @BeforeClass
    public static void beforeClass() {
        NAMED_COMPARATORS.add(null);
    }

    @AfterClass
    public static void afterClass() {
        NAMED_COMPARATORS.remove(null);
    }
}
