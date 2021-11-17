package info.kgeorgiy.java.advanced.hello;

import info.kgeorgiy.java.advanced.base.BaseTest;
import org.junit.Assert;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import java.net.DatagramSocket;
import java.net.SocketException;
import java.util.concurrent.atomic.AtomicInteger;

/**
 * Full tests for {@link HelloClient}.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class HelloClientTest extends BaseTest {
    private static final int PORT = 28888;
    public static final String PREFIX = HelloClientTest.class.getName();

    @Test
    public void test01_singleRequest() throws SocketException {
        test(1, 1, 1);
    }

    @Test
    public void test02_sequence() throws SocketException {
        test(100, 1, 1);
    }

    @Test
    public void test03_singleWithFailures() throws SocketException {
        test(1, 1, 0.1);
    }

    @Test
    public void test04_sequenceWithFailures() throws SocketException {
        test(20, 1, 0.5);
    }

    @Test
    public void test05_singleMultithreaded() throws SocketException {
        test(1, 10, 1);
    }

    @Test
    public void test06_sequenceMultithreaded() throws SocketException {
        test(10, 10, 1);
    }

    @Test
    public void test07_sequenceMultithreadedWithFails() throws SocketException {
        test(10, 10, 0.5);
    }

    private static void test(final int requests, final int threads, final double p) throws SocketException {
        final long start = System.currentTimeMillis();
        final AtomicInteger[] expected;
        try (final DatagramSocket socket = new DatagramSocket(PORT)) {
            expected = Util.server(PREFIX, threads, p, socket);
            final HelloClient client = createCUT();
            client.run("localhost", PORT, PREFIX, threads, requests);
        }
        for (int i = 0; i < expected.length; i++) {
            Assert.assertEquals("Invalid number of requests on thread " + i , requests, expected[i].get());
        }
        System.err.printf("Test finished in %.3fs%n", (System.currentTimeMillis() - start) / 1000.0);
    }
}
