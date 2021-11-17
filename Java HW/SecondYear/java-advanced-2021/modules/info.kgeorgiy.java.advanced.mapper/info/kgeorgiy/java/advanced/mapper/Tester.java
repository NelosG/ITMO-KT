package info.kgeorgiy.java.advanced.mapper;

import info.kgeorgiy.java.advanced.base.BaseTester;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class Tester extends BaseTester {
    public static void main(final String... args) {
        new Tester()
                .add("scalar", ScalarMapperTest.class)
                .add("list", ListMapperTest.class)
                .add("advanced", AdvancedMapperTest.class)
                .run(args);
    }
}
