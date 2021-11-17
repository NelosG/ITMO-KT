package info.kgeorgiy.java.advanced.hello;

import info.kgeorgiy.java.advanced.base.BaseTester;

import java.util.List;

/**
 * Tester for <a href="https://www.kgeorgiy.info/courses/java-advanced/homeworks.html#homework-hello-udp">Hello UDP</a> homework
 * of <a href="https://www.kgeorgiy.info/courses/java-advanced/">Java Advanced</a> course.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public final class Tester {
    public static void main(final String... args) {
        if (args.length > 0) {
            Util.setMode(args[0]);
        }

        final BaseTester tester = new BaseTester();
        for (final String suffix : List.of("", "-i18n", "-evil")) {
            tester.add("server" + suffix, HelloServerTest.class);
            tester.add("client" + suffix, HelloClientTest.class);
        }

        if (args.length >= 2 && args[0].startsWith("both")) {
            final String prefix = args[1];
            run(tester, prefix, "Client", args);
            run(tester, prefix, "Server", args);
        } else {
            tester.run(args);
        }
    }

    private static void run(final BaseTester tester, final String prefix, final String suffix, final String[] args) {
        args[0] = suffix.toLowerCase();
        args[1] = prefix + suffix;
        tester.run(args);
    }
}
