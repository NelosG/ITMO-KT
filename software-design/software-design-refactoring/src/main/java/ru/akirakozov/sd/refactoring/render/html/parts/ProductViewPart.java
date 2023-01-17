package ru.akirakozov.sd.refactoring.render.html.parts;


import ru.akirakozov.sd.refactoring.entity.Product;

public class ProductViewPart extends HtmlViewPart {
    private final Product product;

    public ProductViewPart(Product product) {
        this.product = product;
    }

    public static ProductViewPart of(Product product) {
        return new ProductViewPart(product);
    }

    @Override
    public String getHtml() {
        return product.getName() + "\t" + product.getPrice() + "</br>";
    }
}