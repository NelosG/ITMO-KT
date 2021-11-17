/**
 * Tests for <a href="https://www.kgeorgiy.info/courses/java-advanced/homeworks.html#homework-hello-udp">Hello UDP</a> homework
 * of <a href="https://www.kgeorgiy.info/courses/java-advanced/">Java Advanced</a> course.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
module info.kgeorgiy.java.advanced.hello {
    requires transitive info.kgeorgiy.java.advanced.base;

    exports info.kgeorgiy.java.advanced.hello;

    opens info.kgeorgiy.java.advanced.hello to junit;
}
