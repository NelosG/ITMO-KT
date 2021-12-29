package types;

public class Element implements ExtendedElement {
    final String name;

    public Element(String name) {
        this.name = name;
    }

    public String getName() {
        return name;
    }
}
