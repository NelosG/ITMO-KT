package ru.akirakozov.sd.refactoring.view;

import ru.akirakozov.sd.refactoring.entity.Product;
import ru.akirakozov.sd.refactoring.render.html.HtmlMarkupRenderer;
import ru.akirakozov.sd.refactoring.render.html.parts.HtmlViewPart;
import ru.akirakozov.sd.refactoring.render.html.parts.ProductViewPart;

public class MaxQueryView extends HtmlMarkupRenderer {
    private static final HtmlViewPart LABEL = HtmlViewPart.of("<h1>Product with max price: </h1>");

    public MaxQueryView(Product product) {
        super(LABEL, ProductViewPart.of(product));
    }

    public MaxQueryView() {
        super(LABEL);
    }
}