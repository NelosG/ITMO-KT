package ru.akirakozov.sd.refactoring.view;

import ru.akirakozov.sd.refactoring.entity.Product;
import ru.akirakozov.sd.refactoring.render.html.HtmlMarkupRenderer;
import ru.akirakozov.sd.refactoring.render.html.parts.HtmlViewPart;
import ru.akirakozov.sd.refactoring.render.html.parts.ProductViewPart;

public class MinQueryView extends HtmlMarkupRenderer {
    private static final HtmlViewPart LABEL = HtmlViewPart.of("<h1>Product with min price: </h1>");

    public MinQueryView(Product product) {
        super(LABEL, ProductViewPart.of(product));
    }

    public MinQueryView() {
        super(LABEL);
    }
}