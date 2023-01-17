package ru.akirakozov.sd.refactoring.render.html;

import ru.akirakozov.sd.refactoring.render.Renderer;
import ru.akirakozov.sd.refactoring.render.html.parts.HtmlViewPart;

import java.util.Arrays;
import java.util.stream.Collectors;

public abstract class HtmlRenderer implements Renderer {
    protected HtmlViewPart[] parts;

    protected HtmlRenderer(HtmlViewPart... parts) {
        this.parts = parts;
    }

    @Override
    public String render() {
        return Arrays.stream(parts).map(HtmlViewPart::getHtml).collect(Collectors.joining("\n"));
    }
}