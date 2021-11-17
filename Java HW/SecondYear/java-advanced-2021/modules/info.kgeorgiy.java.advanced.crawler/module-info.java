/**
 * Tests for <a href="https://www.kgeorgiy.info/courses/java-advanced/homeworks.html#homework-crawler">Web Crawler</a> homework
 * of <a href="https://www.kgeorgiy.info/courses/java-advanced/">Java Advanced</a> course.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
module info.kgeorgiy.java.advanced.crawler {
    requires transitive info.kgeorgiy.java.advanced.base;
    requires jsoup;

    exports info.kgeorgiy.java.advanced.crawler;

    opens info.kgeorgiy.java.advanced.crawler to junit;
}
