package info.kgeorgiy.ja.pushkarev.crawler;

import java.util.Set;

public class ControlSet implements Control {

    private final Set<String> dataForCheck;

    // :NOTE: no need for a supplier

    //FIXED
    ControlSet(final Set<String> set) {
        dataForCheck = set;
    }

    @Override
    public boolean check(final String s) {
        return dataForCheck.add(s);
    }
}
