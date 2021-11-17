package info.kgeorgiy.java.advanced.arrayset;

import net.java.quickcheck.collection.Pair;
import org.junit.Assert;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import java.util.*;
import java.util.function.BiFunction;
import java.util.function.Function;

import static net.java.quickcheck.generator.CombinedGeneratorsIterables.somePairs;
import static net.java.quickcheck.generator.PrimitiveGenerators.fixedValues;
import static org.junit.Assert.assertEquals;

/**
 * Tests for hard version
 * of <a href="https://www.kgeorgiy.info/courses/java-advanced/homeworks.html#homework-arrayset">ArraySet</a> homework
 * for <a href="https://www.kgeorgiy.info/courses/java-advanced/">Java Advanced</a> course.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class NavigableSetTest extends SortedSetTest {

    public static final List<Integer> TEST_DESCENDING_SET_DATA = List.of(10, 20, 30);

    @Test
    public void test31_lower() {
        testElement("lower(%s)", NavigableSet::lower);
    }

    @Test
    public void test32_ceiling() {
        testElement("ceiling(%s)", NavigableSet::ceiling);
    }

    @Test
    public void test33_higher() {
        testElement("higher(%s)", NavigableSet::higher);
    }

    @Test
    public void test34_floor() {
        testElement("floor(%s)", NavigableSet::floor);
    }

    @Test
    public void test35_navigableTailSet() {
        testElement("tailSet(%s, true)", (s, e) -> s.tailSet(e, true));
        testElement("tailSet(%s, false)", (s, e) -> s.tailSet(e, false));
    }

    @Test
    public void test36_navigableHeadSet() {
        testElement("headSet(%s, true)", (s, e) -> s.headSet(e, true));
        testElement("headSet(%s, false)", (s, e) -> s.headSet(e, false));
    }

    @Test
    public void test37_navigableSubSet() {
        testN((elements, comparator, treeSet, set, context) -> {
            final Collection<Integer> all = inAndOut(elements);
            for (final Pair<Integer, Integer> p : somePairs(fixedValues(all), fixedValues(all))) {
                final Integer from = p.getFirst();
                final Integer to = p.getSecond();
                if (compare(comparator, from, to)) {
                    for (int i = 0; i < 4; i++) {
                        assertEq(
                                set.subSet(from, i % 2 == 1, to, i / 2 == 1),
                                treeSet.subSet(from, i % 2 == 1, to, i / 2 == 1),
                                String.format("in subSet(%d, %b, %d, %b) (comparator = %s, elements = %s",
                                        from, i % 2 == 1,
                                        to, i / 2 == 1,
                                        comparator, elements
                                )
                        );
                    }
                }
            }
        });
    }

    @Test
    public void test39_descendingSet() {
        testDescendingSet(treeSet(TEST_DESCENDING_SET_DATA, Integer::compareUnsigned), set(TEST_DESCENDING_SET_DATA, Integer::compareUnsigned));
    }

    protected static void testDescendingSet(final NavigableSet<Integer> expected, final NavigableSet<Integer> actual) {
        final SetPair pair = new SetPair(expected.descendingSet(), actual.descendingSet());

        pair.testGet("toArray()", NavigableSetTest::toArray);
        pair.testGet("size()", NavigableSet::size);
        pair.testGet("first()", SortedSet::first);
        pair.testGet("last()", SortedSet::last);
        pair.testGet("descendingIterator().next()", s -> s.descendingIterator().next());

        pair.testGet("floor(%s)", NavigableSet::floor);
        pair.testGet("lower(%s)", NavigableSet::lower);
        pair.testGet("ceiling(%s)", NavigableSet::ceiling);
        pair.testGet("higher(%s)", NavigableSet::higher);

        pair.testGet("headSet(%s).size()", (s, e) -> s.headSet(e).size());
        pair.testGet("tailSet(%s).size()", (s, e) -> s.tailSet(e).size());

        pair.testGet("descendingSet().toArray()", s -> toArray(s.descendingSet()));
    }

    private static class SetPair {
        final NavigableSet<Integer> expected, actual;

        public SetPair(final NavigableSet<Integer> expected, final NavigableSet<Integer> actual) {
            this.expected = expected;
            this.actual = actual;
        }

        private <T> void testGet(
                final String format,
                final BiFunction<NavigableSet<Integer>, Integer, T> method
        ) {
            for (final int element : inAndOut(expected)) {
                assertEquals(String.format(format, element), method.apply(expected, element), method.apply(actual, element));
            }
        }

        private <T> void testGet(final String description, final Function<NavigableSet<Integer>, T> method) {
            try {
                final T expected = method.apply(this.expected);
                assertEquals(description, expected, method.apply(actual));
            } catch (final NoSuchElementException e) {
                try {
                    method.apply(actual);
                    Assert.fail("NoSuchElementException expected");
                } catch (final NoSuchElementException ignore) {
                    // Success
                }
            }
        }
    }

    protected static void testN(final TestCase<Integer, NavigableSet<Integer>> testCase) {
        test(testCase);
    }

    private static <R> void testElement(final String name, final BiFunction<NavigableSet<Integer>, Integer, R> f) {
        testN((elements, comparator, treeSet, set, context) -> {
            for (final Integer element : inAndOut(elements)) {
                assertEquals(
                        String.format("in %s %s", String.format(name, element), context),
                        f.apply(treeSet, element),
                        f.apply(set, element)
                );
            }
        });
    }
}
