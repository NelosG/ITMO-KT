import org.jetbrains.annotations.NotNull;

/**
 * В теле класса решения разрешено использовать только финальные переменные типа RegularInt.
 * Нельзя volatile, нельзя другие типы, нельзя блокировки, нельзя лазить в глобальные переменные.
 *
 * @author Pushkarev Gleb
 */
public class Solution implements MonotonicClock {
    private final RegularInt c1 = new RegularInt(0);
    private final RegularInt c2 = new RegularInt(0);
    private final RegularInt c3 = new RegularInt(0);

    private final RegularInt c1Rev = new RegularInt(0);
    private final RegularInt c2Rev = new RegularInt(0);


    @Override
    public void write(@NotNull Time time) {
        // write left-to-right
        c1.setValue(time.getD1());
        c2.setValue(time.getD2());
        c3.setValue(time.getD3());

        // write right-to-left
        c2Rev.setValue(time.getD2());
        c1Rev.setValue(time.getD1());

    }

    @NotNull
    @Override
    public Time read() {
        // read left-to-right
        int c1RevV = c1Rev.getValue();
        int c2RevV = c2Rev.getValue();
        // read right-to-left
        int c3V = c3.getValue();
        int c2V = c2.getValue();
        int c1V = c1.getValue();

        return new Time(c1V, c1V == c1RevV ? c2V : 0, c1V == c1RevV && c2V == c2RevV ? c3V : 0);
    }
}
