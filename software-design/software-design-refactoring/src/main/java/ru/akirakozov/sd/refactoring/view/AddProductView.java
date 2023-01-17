package ru.akirakozov.sd.refactoring.view;

import ru.akirakozov.sd.refactoring.render.html.HtmlRenderer;
import ru.akirakozov.sd.refactoring.render.html.parts.HtmlViewPart;

public class AddProductView extends HtmlRenderer {
    public AddProductView() {
        super(HtmlViewPart.of("OK"));
    }
}