package info.kgeorgiy.java.advanced.crawler;

import org.junit.Assert;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import java.io.IOException;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class HardCrawlerTest extends EasyCrawlerTest {
    @Test
    public void test10_singleConnectionPerHost() throws IOException {
        test("https://itmo.ru", 2, Integer.MAX_VALUE, Integer.MAX_VALUE, 1, 10, 10);
    }

    @Test
    public void test11_limitedConnectionsPerHost() throws IOException {
        test("https://itmo.ru", 2, Integer.MAX_VALUE, Integer.MAX_VALUE, 10, 10, 10);
    }

    @Test
    public void test12_limitedConnectionsPerformance() throws IOException {
        testPerformance(3, 1500);
        testPerformance(10, 600);
    }

    private static void testPerformance(final int perHost, final double target) throws IOException {
        final long time = test("https://itmo.ru", 2, Integer.MAX_VALUE, Integer.MAX_VALUE, perHost, 100, 50);
        System.err.println("Time: " + time);
        Assert.assertTrue("Too parallel: " + time, time > 0.8 * target);
        Assert.assertTrue("Not parallel: " + time, time < 1.2 * target);
    }
}
