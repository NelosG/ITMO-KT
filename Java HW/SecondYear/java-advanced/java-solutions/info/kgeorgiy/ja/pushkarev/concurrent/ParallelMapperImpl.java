package info.kgeorgiy.ja.pushkarev.concurrent;

import info.kgeorgiy.java.advanced.mapper.ParallelMapper;

import java.util.*;
import java.util.function.Consumer;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class ParallelMapperImpl implements ParallelMapper {

    private final TaskQueue tasks = new TaskQueue();
    private final List<Thread> threads;
    private volatile boolean closed = false;


    public ParallelMapperImpl(final int threads) {
        final Runnable runner = () -> {
            try {
                while (!Thread.currentThread().isInterrupted()) {
                    tasks.nextSubTask().run();
                }
            } catch (final InterruptedException ignored) {
                Thread.currentThread().interrupt();
            }
        };
        this.threads = Stream.generate(() -> new Thread(runner))
                .limit(threads)
                .collect(Collectors.toList());
        this.threads.forEach(Thread::start);
    }

    @Override
    public <T, R> List<R> map(final Function<? super T, ? extends R> f,
                              final List<? extends T> args) throws InterruptedException {
        if (closed) {
            throw new IllegalStateException("ParallelMapperImpl closed.");
        }

        if (args.isEmpty()) {
            return List.of();
        }
        final Task<T, R> task = new Task<>(f, args);
        tasks.add(task);
        return task.getResult();
    }

    @Override
    public void close() {
        if (!closed) {
            closed = true;
            tasks.forEach(Task::terminate);
            synchronized (threads) {
                threads.forEach(Thread::interrupt);
                joinThreads();
            }
        }
    }

    private void joinThreads() {
        for (int i = 0; i < threads.size(); i++) {
            try {
                threads.get(i).join();
            } catch (final InterruptedException e) {
                --i;
            }
        }
    }

    private class TaskQueue extends ArrayDeque<Task<?, ?>> {

        public TaskQueue() {
            super();
        }

        public synchronized boolean add(final Task<?, ?> task) {
            boolean result = super.add(task);
            notifyAll();
            return result;
        }

        @Override
        public synchronized void forEach(Consumer<? super Task<?, ?>> action) {
            super.forEach(action);
        }

        @Override
        public synchronized Task<?, ?> remove() {
            return super.remove();
        }

        public synchronized Runnable nextSubTask() throws InterruptedException {
            while (isEmpty()) {
                wait();
            }
            final Task<?, ?> task = element();
            Runnable subTask = task.getSubTask();
            return () -> {
                subTask.run();
                task.subTaskFinished();
            };
        }
    }

    private class Task<T, R> {

        public final Queue<Runnable> subTasks = new ArrayDeque<>();
        private final List<R> result;
        private volatile RuntimeException re = null;
        private volatile boolean terminated = false;
        private int notFinished;
        private int forStart;

        public Task(final Function<? super T, ? extends R> f, final List<? extends T> args) {
            result = new ArrayList<>(Collections.nCopies(args.size(), null));

            notFinished = forStart = args.size();
            for (int i = 0; i < args.size(); i++) {
                int index = i;
                subTasks.add(() -> {
                    try {
                        if (!terminated) {
                            result.set(index, f.apply(args.get(index)));
                        }
                    } catch (final RuntimeException e) {
                        addException(e, index);
                    }
                });
            }
        }

        public synchronized Runnable getSubTask() {
            if (--forStart == 0) {
                tasks.remove();
            }
            return subTasks.remove();
        }

        private void addException(RuntimeException e, int index) {
            if (!terminated) {
                RuntimeException my = new RuntimeException("Value on index: " + index + " isn't correct because of", e);
                if (re == null) {
                    re = my;
                } else {
                    re.addSuppressed(my);
                }
            }
        }

        public synchronized void subTaskFinished() {
            if (--notFinished == 0) {
                terminate();
            }
        }

        private void throwEx() {
            if (re != null) {
                throw re;
            }
        }

        public void clearEx() {
            re = null;
        }

        public synchronized List<R> getResult() throws InterruptedException {
            while (!terminated && !closed) {
                wait();
            }
            if (!terminated) {
                terminate();
            }
            throwEx();
            return result;
        }

        public synchronized void terminate() {
            terminated = true;
            notifyAll();
        }
    }
}
