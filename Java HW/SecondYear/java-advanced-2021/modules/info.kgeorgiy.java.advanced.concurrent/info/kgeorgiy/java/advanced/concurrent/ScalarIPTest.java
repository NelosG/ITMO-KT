package info.kgeorgiy.java.advanced.concurrent;

import org.junit.Assert;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.stream.Stream;

/**
 * Full tests for easy version
 * of <a href="https://www.kgeorgiy.info/courses/java-advanced/homeworks.html#homework-concurrent">Iterative parallelism</a> homework
 * for <a href="https://www.kgeorgiy.info/courses/java-advanced/">Java Advanced</a> course.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class ScalarIPTest<P extends ScalarIP> extends BaseIPTest<P> {
    public static final int PROCESSORS = Runtime.getRuntime().availableProcessors();

    public static final Comparator<Integer> BURN_COMPARATOR = (o1, o2) -> {
        int total = o1 + o2;
        for (int i = 0; i < 10_000_000; i++) {
            total += i;
        }
        if (total == o1 + o2) {
            throw new AssertionError();
        }
        return Integer.compare(o1, o2);
    };

    public static final Comparator<Integer> SLEEP_COMPARATOR = (o1, o2) -> {
        try {
            Thread.sleep(100);
        } catch (final InterruptedException e) {
            e.printStackTrace();
            Thread.currentThread().interrupt();
        }
        return Integer.compare(o1, o2);
    };

    @Test
    public void test01_maximum() throws InterruptedException {
        test(Collections::max, ScalarIP::maximum, COMPARATORS);
    }

    @Test
    public void test02_minimum() throws InterruptedException {
        test(Collections::min, ScalarIP::minimum, COMPARATORS);
    }

    @Test
    public void test03_all() throws InterruptedException {
        testS(Stream::allMatch, ScalarIP::all, PREDICATES);
    }

    @Test
    public void test04_any() throws InterruptedException {
        testS(Stream::anyMatch, ScalarIP::any, PREDICATES);
    }

    @Test
    public void test05_sleepPerformance() throws InterruptedException {
        testPerformance(randomList(50), SLEEP_COMPARATOR, 10, 2);
    }

    @Test
    public void test06_burnPerformance() throws InterruptedException {
        testPerformance(randomList(100 * PROCESSORS), BURN_COMPARATOR, PROCESSORS, 1.5);
    }

    private void testPerformance(final List<Integer> data, final Comparator<Integer> comparator, final int threads, final double max) throws InterruptedException {
        final double speedup = speedup(data, comparator, threads);
        Assert.assertTrue("Lower bound hit", speedup > 1 / max);
        Assert.assertTrue("Upper bound hit", speedup < max);
    }

    protected double speedup(final List<Integer> data, final Comparator<Integer> comparator, final int threads) throws InterruptedException {
        System.err.println("    Warm up");
        final ConcurrentFunction<P, Integer, Comparator<Integer>> maximum = ScalarIP::maximum;

        final int subtasks = getSubtasks(threads, threads);
        final int tail = data.size() % subtasks;
        if (tail != 0) {
            data.addAll(randomList(subtasks - tail));
        }

        for (int i = 0; i < 5; i++) {
            performance(threads, threads, data, maximum, comparator);
        }
        System.err.println("    Measurement");

        final double performance1 = performance(1, threads, data, maximum, comparator);
        final double performance2 = performance(threads, threads, data, maximum, comparator);
        final double speedup = performance2 / performance1;
        System.err.format("    Performance ratio %.1f for %d threads (%.1f %.1f ms/op)%n", speedup, threads, performance1, performance2);
        return speedup;
    }

    protected int getSubtasks(final int threads, final int totalThreads) {
        return threads;
    }

    private double performance(final int threads, final int totalThreads, final List<Integer> data, final ConcurrentFunction<P, Integer, Comparator<Integer>> f, final Comparator<Integer> comparator) throws InterruptedException {
        final int subtasks = getSubtasks(threads, totalThreads);
        assert subtasks % threads == 0 && data.size() % subtasks == 0;

        final List<Integer> copy = List.copyOf(data);
        final P instance = createInstance(threads);

        final long start = System.nanoTime();
        f.apply(instance, subtasks, copy, comparator);
        final long time = System.nanoTime() - start;

        final int sequential = subtasks - 1;
        final int parallel = (data.size() - subtasks) / threads;
        return time / 1e6 / (sequential + parallel);
    }
}
