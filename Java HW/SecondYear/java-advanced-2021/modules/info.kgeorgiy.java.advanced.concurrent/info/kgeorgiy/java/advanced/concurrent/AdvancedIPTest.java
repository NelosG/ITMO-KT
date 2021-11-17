package info.kgeorgiy.java.advanced.concurrent;

import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import java.util.List;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.stream.Stream;

/**
 * Full tests for advanced version
 * of <a href="https://www.kgeorgiy.info/courses/java-advanced/homeworks.html#homework-mapper">Parallel Mapper</a> homework
 * for <a href="https://www.kgeorgiy.info/courses/java-advanced/">Java Advanced</a> course.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class AdvancedIPTest extends ListIPTest<AdvancedIP> {
    public static final List<Named<AdvancedIP.Monoid<Integer>>> MONOIDS = List.of(
            monoid("add", 0, Integer::sum),
            monoid("mul", 1, (a, b) -> a * b),
            monoid("min", Integer.MAX_VALUE, Integer::min)
    );

    @Test
    public void test71_reduce() throws InterruptedException {
        testS(AdvancedIPTest::reduce, AdvancedIP::reduce, MONOIDS);
    }

    private static <T> T reduce(final Stream<T> data, final AdvancedIP.Monoid<T> m) {
        return data.reduce(m.getIdentity(), m.getOperator());
    }

    @SafeVarargs
    @SuppressWarnings("varargs")
    private <T> void testM(final Named<MappedMonoid<T>>... cases) throws InterruptedException {
        testS(
                (data, m) -> m.apply(data),
                (i, t, d, m) -> i.mapReduce(t, d, m.lift, m.monoid),
                List.of(cases)
        );
    }

    @Test
    public void test72_mapReduce() throws InterruptedException {
        testM(
                mappedMonoid("*2-add", a -> a * 2, 0, Integer::sum),
                mappedMonoid("+2-mul", a -> a + 2, 1, (a, b) -> a * b)
        );
        testM(
                mappedMonoid("toString-min", Object::toString, "9".repeat(20), BinaryOperator.minBy(String::compareTo)),
                mappedMonoid("toString-concat", Object::toString, "", String::concat)
        );
    }


    private static <T> Named<AdvancedIP.Monoid<T>> monoid(final String name, final T identity, final BinaryOperator<T> operator) {
        return named(name, new AdvancedIP.Monoid<>(identity, operator));
    }

    private static <T> Named<MappedMonoid<T>> mappedMonoid(final String name, final Function<Integer, T> lift, final T identity, final BinaryOperator<T> operator) {
        return named(name, new MappedMonoid<>(lift, new AdvancedIP.Monoid<>(identity, operator)));
    }

    private static class MappedMonoid<R> {
        private final Function<Integer, R> lift;
        private final AdvancedIP.Monoid<R> monoid;

        public MappedMonoid(final Function<Integer, R> lift, final AdvancedIP.Monoid<R> monoid) {
            this.lift = lift;
            this.monoid = monoid;
        }

        private R apply(final Stream<Integer> data) {
            return reduce(data.map(lift), monoid);
        }
    }
}
