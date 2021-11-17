package expression.parser;

import java.util.List;

/**
 * @author Georgiy Korneev
 */
public class ParserShiftsTest extends ParserTest {
    protected ParserShiftsTest() {
        levels.add(0, list(
                op("<<", (a, b) -> (int) a << (int) b),
                op(">>", (a, b) -> (int) a >> (int) b)
        ));

        tests.addAll(List.of(
                op("1 << 5 + 3", (x, y, z) -> 256L),
                op("x + y << z", (x, y, z) -> (int) (x + y) << (int) z),
                op("x * y << z", (x, y, z) -> (int) (x * y) << (int) z),
                op("x << y << z", (x, y, z) -> (int) x << (int) y << (int) z),
                op("1024 >> 5 + 3", (x, y, z) -> 4L),
                op("x + y >> z", (x, y, z) -> x + y >> z),
                op("x * y >> z", (x, y, z) -> x * y >> z),
                op("x >> y >> z", (x, y, z) -> x >> y >> z)
        ));
    }

    public static void main(final String[] args) {
        new ParserShiftsTest().run();
    }
}
