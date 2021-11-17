package info.kgeorgiy.java.advanced.implementor;

import info.kgeorgiy.java.advanced.implementor.full.interfaces.CovariantReturns;
import org.junit.Test;

/**
 * Full tests for covariant version
 * of <a href="https://www.kgeorgiy.info/courses/java-advanced/homeworks.html#homework-implementor">Implementor</a> homework
 * for <a href="https://www.kgeorgiy.info/courses/java-advanced/">Java Advanced</a> course.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class CovariantImplementorTest extends AdvancedImplementorTest {
    @Test
    public void test45_covariant() {
        test(false, CovariantReturns.OK);
    }
}
