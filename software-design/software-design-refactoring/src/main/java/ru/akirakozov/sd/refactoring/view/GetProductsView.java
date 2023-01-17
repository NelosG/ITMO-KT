package ru.akirakozov.sd.refactoring.view;

import ru.akirakozov.sd.refactoring.entity.Product;
import ru.akirakozov.sd.refactoring.render.html.HtmlMarkupRenderer;
import ru.akirakozov.sd.refactoring.render.html.parts.HtmlViewPart;
import ru.akirakozov.sd.refactoring.render.html.parts.ProductViewPart;

import java.util.List;

public class GetProductsView extends HtmlMarkupRenderer {
    public GetProductsView(List<Product> products) {
        super(products.stream().map(ProductViewPart::of).toArray(HtmlViewPart[]::new));
    }
}