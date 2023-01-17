package ru.akirakozov.sd.refactoring.view;

import ru.akirakozov.sd.refactoring.render.html.HtmlMarkupRenderer;
import ru.akirakozov.sd.refactoring.render.html.parts.HtmlViewPart;

public class CountQueryView extends HtmlMarkupRenderer {
    public CountQueryView(int count) {
        super(HtmlViewPart.of("Number of products: "), HtmlViewPart.of(count));
    }
}