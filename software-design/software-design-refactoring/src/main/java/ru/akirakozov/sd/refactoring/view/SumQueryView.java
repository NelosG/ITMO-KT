package ru.akirakozov.sd.refactoring.view;

import ru.akirakozov.sd.refactoring.render.html.HtmlMarkupRenderer;
import ru.akirakozov.sd.refactoring.render.html.parts.HtmlViewPart;

public class SumQueryView extends HtmlMarkupRenderer {
    public SumQueryView(long sum) {
        super(HtmlViewPart.of("Summary price: "), HtmlViewPart.of(sum));
    }
}