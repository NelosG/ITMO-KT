package ru.akirakozov.sd.refactoring.render.html.parts;

public abstract class HtmlViewPart {
    public static HtmlViewPart of(Object value) {
        return new HtmlViewPart() {
            @Override
            public String getHtml() {
                return String.valueOf(value);
            }
        };
    }

    public abstract String getHtml();

    @Override
    public String toString() {
        return getHtml();
    }
}