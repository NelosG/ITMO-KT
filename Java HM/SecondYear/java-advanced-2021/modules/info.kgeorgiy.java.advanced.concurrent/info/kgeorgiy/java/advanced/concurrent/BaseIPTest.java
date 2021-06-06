package info.kgeorgiy.java.advanced.concurrent;

import info.kgeorgiy.java.advanced.base.BaseTest;
import org.junit.Assert;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Random;
import java.util.function.BiFunction;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.IntStream;
import java.util.stream.Stream;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class BaseIPTest<P extends ScalarIP> extends BaseTest {
    public static final List<Integer> SIZES = List.of(10_000, 5, 2, 1);
    private final Random random = new Random(3257083275083275083L);
    protected List<Integer> factors = Collections.singletonList(0);

    protected final <T, U> void test(final BiFunction<List<Integer>, U, T> fExpected, final ConcurrentFunction<P, T, U> fActual, final List<Named<U>> cases) throws InterruptedException {
        for (final int factor : factors) {
            final P instance = createInstance(factor);
            for (final int n : SIZES) {
                System.err.println("    --- Size " + n);
                final List<Integer> data = randomList(n);
                for (final Named<U> named : cases) {
                    final T expected = fExpected.apply(data, named.value);
                    System.err.print("        " + named.name + ", threads: ");
                    for (int threads = 1; threads <= 10; threads++) {
                        System.err.print(" " + threads);
                        Assert.assertEquals(threads + " threads", expected, fActual.apply(instance, threads, data, named.value));
                    }
                    System.err.println();
                }
                System.err.println();
            }
        }
    }

    protected final <T, U> void testS(final BiFunction<Stream<Integer>, U, T> fExpected, final ConcurrentFunction<P, T, U> fActual, final List<Named<U>> cases) throws InterruptedException {
        test((data, value) -> fExpected.apply(data.stream(), value), fActual, cases);
    }

    protected final List<Integer> randomList(final int size) {
        final int[] pool = random.ints(Math.min(size, 1000_000)).toArray();
        return IntStream.generate(() -> pool[random.nextInt(pool.length)]).limit(size).boxed().collect(Collectors.toList());
    }

    protected P createInstance(final int threads) {
        return createCUT();
    }

    public interface ConcurrentFunction<P, T, U> {
        T apply(P instance, int threads, List<Integer> data, U value) throws InterruptedException;
    }

    protected static final class Named<T> {
        public final String name;
        public final T value;

        private Named(final String name, final T value) {
            this.name = name;
            this.value = value;
        }
    }

    protected static <T> Named<T> named(final String name, final T value) {
        return new Named<>(name, value);
    }

    protected static final List<Named<Comparator<Integer>>> COMPARATORS = List.of(
            named("Natural order", Integer::compare),
            named("Reverse order", (l1, l2) -> Integer.compare(l2, l1)),
            named("Div 100", Comparator.comparingInt(v -> v / 100)),
            named("Even first", Comparator.<Integer>comparingInt(v -> v % 2).thenComparing(v -> v)),
            named("All equal", (v1, v2) -> 0)
    );

    protected static final List<Named<Predicate<Integer>>> PREDICATES = List.of(
            named("Equal 0", Predicate.isEqual(0)),
            named("Greater than 0", i -> i > 0),
            named("Even", i -> i % 2 == 0),
            named("True", i -> true),
            named("False", i -> false)
    );

    protected static final List<Named<Function<Integer, ?>>> FUNCTIONS = List.of(
            named("* 2", v -> v * 2),
            named("is even", v -> v % 2 == 0),
            named("toString", Object::toString)
    );

    protected static final List<Named<Void>> UNIT = List.of(named("Common", null));
}
