package info.kgeorgiy.java.advanced.mapper;

import info.kgeorgiy.java.advanced.concurrent.ScalarIP;
import info.kgeorgiy.java.advanced.concurrent.ScalarIPTest;
import org.junit.AfterClass;
import org.junit.FixMethodOrder;
import org.junit.runners.MethodSorters;

import java.util.List;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class ScalarMapperTest extends ScalarIPTest<ScalarIP> {
    private static ParallelMapper parallelMapper;
    public ScalarMapperTest() {
        factors = List.of(1, 2, 5, 10, 20);
    }

    static Object create(final String className, final Class<?> argType, final Object value) {
        try {
            final Class<?> clazz = Class.forName(className);
            return clazz.getConstructor(argType).newInstance(value);
        } catch (final Exception e) {
            throw new AssertionError(e);
        }
    }

    @Override
    protected ScalarIP createInstance(final int threads) {
        return instance(threads);
    }

    static ScalarIP instance(final int threads) {
        close();
        final String parallelMapperName = System.getProperty("cut");
        parallelMapper = (ParallelMapper) create(parallelMapperName.replace("IterativeParallelism", "ParallelMapperImpl"), int.class, threads);
        return (ScalarIP) create(parallelMapperName, ParallelMapper.class, parallelMapper);
    }

    @Override
    protected int getSubtasks(final int threads, final int totalThreads) {
        return subtasks(totalThreads);
    }

    protected static int subtasks(final int totalThreads) {
        return totalThreads * 2;
    }

    @AfterClass
    public static void close() {
        if (parallelMapper != null) {
            parallelMapper.close();
        }
    }
}
