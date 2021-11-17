package info.kgeorgiy.java.advanced.concurrent;

import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.Optional;
import java.util.stream.Collectors;

/**
 * Full tests for hard version
 * of <a href="https://www.kgeorgiy.info/courses/java-advanced/homeworks.html#homework-concurrent">Iterative parallelism</a> homework
 * for <a href="https://www.kgeorgiy.info/courses/java-advanced/">Java Advanced</a> course.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class ListIPTest<P extends ListIP> extends ScalarIPTest<P> {
    @Test
    public void test51_join() throws InterruptedException {
        testS(
                (data, ignore) -> data.map(Object::toString).collect(Collectors.joining()),
                (i, t, d, v) -> i.join(t, d),
                UNIT
        );
    }

    @Test
    public void test52_filter() throws InterruptedException {
        testS(
                (data, predicate) -> data.filter(predicate).collect(Collectors.toList()),
                ListIP::filter,
                PREDICATES
        );
    }

    @Test
    public void test53_map() throws InterruptedException {
        testS((data, f) -> data.map(f).collect(Collectors.toList()), ListIP::map, FUNCTIONS);
    }

    @Test
    public void test54_mapMaximum() throws InterruptedException {
        testS(
                (data, f) -> data.map(f).map(Objects::toString).max(Comparator.naturalOrder()),
                (instance, threads, data, f) -> {
                    final List<String> mapped = instance.map(threads, data, f.andThen(Objects::toString));
                    return Optional.of(instance.maximum(threads, mapped, Comparator.naturalOrder()));
                },
                FUNCTIONS
        );
    }
}
