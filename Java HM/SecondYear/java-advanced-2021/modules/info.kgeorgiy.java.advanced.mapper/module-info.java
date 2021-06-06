/**
 * Tests for <a href="https://www.kgeorgiy.info/courses/java-advanced/homeworks.html#homework-mapper">Parallel Mapper</a> homework
 * of <a href="https://www.kgeorgiy.info/courses/java-advanced/">Java Advanced</a> course.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
module info.kgeorgiy.java.advanced.mapper {
    requires transitive info.kgeorgiy.java.advanced.concurrent;

    exports info.kgeorgiy.java.advanced.mapper;

    opens info.kgeorgiy.java.advanced.mapper to junit;
}
