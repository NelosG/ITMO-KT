package info.kgeorgiy.java.advanced.base;

import org.junit.runner.JUnitCore;
import org.junit.runner.Result;
import org.junit.runner.notification.Failure;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.function.BiFunction;

/**
 * Test runners base class.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class BaseTester {
    private final long start = System.currentTimeMillis();
    private final Map<String, BiFunction<BaseTester, String, Class<?>>> tests = new LinkedHashMap<>();

    public void run(final String[] args) {
        if (args.length != 2 && args.length != 3) {
            printUsage();
            return;
        }

        final String test = args[0];
        final String cut = args[1];
        if (!tests.containsKey(test)) {
            printUsage();
            return;
        }

        final Class<?> token = test(test, cut);

        System.out.println("============================");
        final long time = System.currentTimeMillis() - start;
        System.out.printf("OK %s for %s in %dms %n", test, cut, time);
        certify(token, test + (args.length > 2 ? args[2] : ""));
    }

    public Class<?> test(final String test, final String cut) {
        return tests.get(test).apply(this, cut);
    }

    private static Class<?> test(final String cut, final Class<?> test) {
        System.err.printf("Running %s for %s%n", test, cut);

        System.setProperty(BaseTest.CUT_PROPERTY, cut);
        final Result result = new JUnitCore().run(test);
        if (result.wasSuccessful()) {
            return test;
        }

        for (final Failure failure : result.getFailures()) {
            System.err.println("Test " + failure.getDescription().getMethodName() + " failed: " + failure.getMessage());
            if (failure.getException() != null) {
                failure.getException().printStackTrace();
            }
        }
        System.exit(1);
        throw new AssertionError("Exit");
    }

    protected static void certify(final Class<?> token, final String salt) {
        try {
            final CG cg = (CG) Class.forName("info.kgeorgiy.java.advanced.base.CertificateGenerator").getDeclaredConstructor().newInstance();
            cg.certify(token, salt);
        } catch (final ClassNotFoundException e) {
            // Ignore
        } catch (final Exception e) {
            System.err.println("Error running certificate generator");
            e.printStackTrace();
        }
    }

    private void printUsage() {
        System.out.println("Usage:");
        for (final String name : tests.keySet()) {
            System.out.format(
                    "    java -cp . -p . -m %s %s Solution.class.name [salt]%n",
                    getClass().getPackage().getName(),
                    name
            );
        }
        System.exit(1);
    }

    public BaseTester add(final String name, final Class<?> testClass) {
        return add(name, (tester, cut) -> test(cut, testClass));
    }

    public BaseTester add(final String name, final BiFunction<BaseTester, String, Class<?>> test) {
        tests.put(name, test);
        return this;
    }
}
