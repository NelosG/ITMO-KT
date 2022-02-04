package types;

import java.util.List;

public class NonTerm extends Element {

    final List<String> callAttributes;

    public NonTerm(String name, List<String> callAttributes) {
        super(name);
        this.callAttributes = callAttributes;
    }

    public List<String> getCallAttributes() {
        return callAttributes;
    }
}
