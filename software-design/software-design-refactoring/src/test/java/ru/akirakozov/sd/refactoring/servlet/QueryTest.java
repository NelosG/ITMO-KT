package ru.akirakozov.sd.refactoring.servlet;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import ru.akirakozov.sd.refactoring.dao.ProductDao;
import ru.akirakozov.sd.refactoring.entity.Product;

import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;
import java.util.Objects;
import java.util.UUID;
import java.util.concurrent.ThreadLocalRandom;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.class)
public class QueryTest extends AbstractTest {

    private String callServletWithValidationAndGetResult(String command) {
        when(request.getParameter("command")).thenReturn(command);
        return parseAndValidateHtml(callServletWithValidationAndGetResponce());
    }

    private String parseAndValidateHtml(String htmlResponse) {
        htmlResponse = htmlResponse.replaceAll("(\r)?\n", "");

        assertTrue(htmlResponse.startsWith("<html>"));
        assertTrue(htmlResponse.endsWith("</html>"));

        htmlResponse = htmlResponse.substring("<html>".length(), htmlResponse.length() - "</html>".length()).trim();
        assertTrue(htmlResponse.startsWith("<body>"));
        assertTrue(htmlResponse.endsWith("</body>"));

        return htmlResponse.substring("<body>".length(), htmlResponse.length() - "</body>".length()).trim();
    }

    private TestData prepareData(int count) {
        TestData data = new TestData();

        for (int i = 0; i < count; i++) {
            data.products.add(new Product(
                    UUID.randomUUID().toString(),
                    ThreadLocalRandom.current().nextInt(0, Integer.MAX_VALUE)
            ));
        }

        data.products.forEach(this::addProduct);

        return data;
    }

    @Test
    public void testEmptyMax() {
        String result = callServletWithValidationAndGetResult("max");
        assertEquals("<h1>Product with max price: </h1>", result);
    }

    @Test
    public void testEmptyMin() {
        String result = callServletWithValidationAndGetResult("min");
        assertEquals("<h1>Product with min price: </h1>", result);
    }

    @Test
    public void testEmptySum() {
        String result = callServletWithValidationAndGetResult("sum");
        assertEquals("Summary price: 0", result);
    }

    @Test
    public void testEmptyCount() {
        String result = callServletWithValidationAndGetResult("count");
        assertEquals("Number of products: 0", result);
    }


    @Test
    public void basicTest() {
        TestData testData = prepareData(5);

        String result = callServletWithValidationAndGetResult("count");
        assertEquals("Number of products: " + testData.products.size(), result);

        result = callServletWithValidationAndGetResult("sum");
        assertEquals("Summary price: " + testData.getPriceSum(), result);

        result = callServletWithValidationAndGetResult("min");
        assertEquals("<h1>Product with min price: </h1>" + testData.getMinPriceProduct().toString() + "</br>", result);

        result = callServletWithValidationAndGetResult("max");
        assertEquals("<h1>Product with max price: </h1>" + testData.getMaxPriceProduct().toString() + "</br>", result);
    }

    @Test
    public void randomTest() {
        TestData testData = prepareData(100);

        String result = callServletWithValidationAndGetResult("count");
        assertEquals("Number of products: " + testData.products.size(), result);

        result = callServletWithValidationAndGetResult("sum");
        assertEquals("Summary price: " + testData.getPriceSum(), result);

        result = callServletWithValidationAndGetResult("min");
        assertEquals("<h1>Product with min price: </h1>" + testData.getMinPriceProduct() + "</br>", result);

        result = callServletWithValidationAndGetResult("max");
        assertEquals("<h1>Product with max price: </h1>" + testData.getMaxPriceProduct() + "</br>", result);
    }

    @Override
    void runSupport() {
        try {
            new QueryServlet(new ProductDao()).doGet(request, response);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    List<Product> getProductsFromResponce(String htmlResponse) {
        return null;
    }

    private static class TestData {
        private final List<Product> products = new ArrayList<>();

        private long getPriceSum() {
            return products.stream().mapToLong(Product::getPrice).sum();
        }

        private Product getMinPriceProduct() {
            return products.stream().min(Comparator.comparing(Product::getPrice)).orElse(null);
        }

        private Product getMaxPriceProduct() {
            return products.stream().max(Comparator.comparing(Product::getPrice)).orElse(null);
        }


        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (!(o instanceof TestData)) return false;
            TestData testData = (TestData) o;
            return products.equals(testData.products);
        }

        @Override
        public int hashCode() {
            return Objects.hash(products);
        }
    }
}