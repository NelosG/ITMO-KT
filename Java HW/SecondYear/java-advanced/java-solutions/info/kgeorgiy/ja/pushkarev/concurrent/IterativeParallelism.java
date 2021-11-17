package info.kgeorgiy.ja.pushkarev.concurrent;

import info.kgeorgiy.java.advanced.concurrent.AdvancedIP;
import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.function.Function;
import java.util.function.Predicate;
import java.util.stream.Collectors;
import java.util.stream.Stream;


public class IterativeParallelism implements AdvancedIP {

    private final ParallelMapper mapper;

    public IterativeParallelism() {
        this.mapper = null;
    }


    public IterativeParallelism(final ParallelMapper mapper) {
        this.mapper = mapper;
    }

    @Override
    public <T> T maximum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
        return resolve(threads, values,
                stream -> stream.max(comparator).orElseThrow(),
                stream -> stream.max(comparator).orElseThrow());
    }

    @Override
    public <T> T minimum(int threads, List<? extends T> values, Comparator<? super T> comparator) throws InterruptedException {
        return maximum(threads, values, Collections.reverseOrder(comparator));
    }

    @Override
    public <T> boolean all(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return resolve(threads, values,
                stream -> stream.allMatch(predicate),
                stream -> stream.allMatch(Boolean::booleanValue)
        );
    }

    @Override
    public <T> boolean any(int threads, List<? extends T> values, Predicate<? super T> predicate) throws InterruptedException {
        return resolve(threads, values,
                stream -> stream.anyMatch(predicate),
                stream -> stream.anyMatch(Boolean::booleanValue)
        );
    }

    @Override
    public String join(int threads, List<?> values) throws InterruptedException {
        return resolve(threads, values,
                stream -> stream.filter(Predicate.not(Objects::isNull)).map(Object::toString).collect(Collectors.joining()),
                stream -> stream.collect(Collectors.joining())
        );
    }

    @Override
    public <T> List<T> filter(final int threads, final List<? extends T> values, final Predicate<? super T> predicate) throws InterruptedException {
        return resolve(threads, values,
                stream -> stream.filter(predicate).collect(Collectors.toList()),
                stream -> stream.flatMap(Collection::stream).collect(Collectors.toList())
        );
    }

    @Override
    public <T, U> List<U> map(final int threads, final List<? extends T> values, final Function<? super T, ? extends U> func) throws InterruptedException {
        return resolve(threads, values,
                stream -> stream.map(func).collect(Collectors.toList()),
                stream -> stream.flatMap(Collection::stream).collect(Collectors.toList())
        );
    }

    @Override
    public <T> T reduce(final int threads, final List<T> values, final Monoid<T> monoid) throws InterruptedException {

        return resolve(threads, values,
                stream -> getReduce(stream, monoid),
                stream -> getReduce(stream, monoid));
    }

    @Override
    public <T, R> R mapReduce(final int threads, final List<T> values,
                              final Function<T, R> lift, final Monoid<R> monoid) throws InterruptedException {

        return resolve(threads, values,
                stream -> getReduce(stream.map(lift), monoid),
                stream -> getReduce(stream, monoid));
    }

    private <T> T getReduce(Stream<T> stream, Monoid<T> monoid) {
        return stream.reduce(monoid.getIdentity(), monoid.getOperator());
    }

    private <T, R> R resolve(final int requestedThreads, final List<T> values,
                             final Function<Stream<T>, R> task,
                             final Function<Stream<R>, R> ansCollector)
            throws InterruptedException {
        if (requestedThreads <= 0) {
            throw new IllegalArgumentException("Number of threads must be > 0");
        }

        final int usingThreads = Math.max(1, Math.min(requestedThreads, values.size()));
        final List<Stream<T>> chunks = split(values, usingThreads);

        return ansCollector.apply(map(task, chunks).stream());
    }

    private <T, R> List<R> map(final Function<Stream<T>, R> task, final List<Stream<T>> chunks) throws InterruptedException {
        if(mapper != null){
            return mapper.map(task, chunks);
        }
        final List<Thread> threads = new ArrayList<>(Collections.nCopies(chunks.size() - 1, null));
        final List<R> result = new ArrayList<>(Collections.nCopies(chunks.size(), null));
        for (int i = 0; i < chunks.size() - 1; i++) {
            final int ind = i;
            Thread thread = new Thread(() -> result.set(ind, task.apply(chunks.get(ind))));
            threads.set(ind, thread);
            thread.start();
        }
        result.set(chunks.size() - 1, task.apply(chunks.get(chunks.size() - 1)));
        joinThreads(threads);
        return result;
    }

    private <T> List<Stream<T>> split(final List<T> values, final int usingThreads) {
        List<Stream<T>> chunks = new ArrayList<>();
        final int blockSize = values.size() / usingThreads;
        int rest = values.size() % usingThreads;
        int r = 0, l;
        for (int i = 0; i < usingThreads; i++) {
            l = r;
            r = l + blockSize + (rest-- > 0 ? 1 : 0);
            chunks.add(values.subList(l, r).stream());
        }
        return chunks;
    }


    private void joinThreads(final List<Thread> threads) throws InterruptedException {
        final InterruptedException exceptions = new InterruptedException();
        for (Thread thread : threads) {
            try {
                thread.join();
            } catch (InterruptedException e) {
                exceptions.addSuppressed(e);
            }
        }
        if (exceptions.getSuppressed().length != 0) {
            throw exceptions;
        }
    }
}
