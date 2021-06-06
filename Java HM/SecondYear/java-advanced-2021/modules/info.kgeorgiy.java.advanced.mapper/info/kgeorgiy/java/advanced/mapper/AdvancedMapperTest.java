package info.kgeorgiy.java.advanced.mapper;

import info.kgeorgiy.java.advanced.concurrent.AdvancedIP;
import info.kgeorgiy.java.advanced.concurrent.AdvancedIPTest;
import org.junit.AfterClass;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import java.util.List;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class AdvancedMapperTest extends AdvancedIPTest {
    public AdvancedMapperTest() {
        factors = List.of(1, 2, 5, 10);
    }

    @Test @Override
    public void test05_sleepPerformance() throws InterruptedException {
        new ScalarMapperTest().test05_sleepPerformance();
    }

    @Override
    protected AdvancedIP createInstance(final int threads) {
        return (AdvancedIP) ScalarMapperTest.instance(threads);
    }

    @Override
    protected int getSubtasks(final int threads, final int totalThreads) {
        return ScalarMapperTest.subtasks(totalThreads);
    }

    @AfterClass
    public static void close() {
        ScalarMapperTest.close();
    }
}
