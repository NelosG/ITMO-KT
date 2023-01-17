package ru.akirakozov.sd.refactoring.servlet;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import ru.akirakozov.sd.refactoring.dao.ProductDao;
import ru.akirakozov.sd.refactoring.entity.Product;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.UUID;
import java.util.concurrent.ThreadLocalRandom;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

@RunWith(MockitoJUnitRunner.class)
public class GetProductTest extends AbstractTest {

    @Override
    void runSupport() {
        try {
            new GetProductsServlet(new ProductDao()).doGet(request, response);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    List<Product> getProductsFromResponce(String htmlResponse) {
        htmlResponse = htmlResponse.replaceAll("(\r)?\n", "");

        assertTrue(htmlResponse.startsWith("<html>"));
        assertTrue(htmlResponse.endsWith("</html>"));

        htmlResponse = htmlResponse.substring("<html>".length(), htmlResponse.length() - "</html>".length()).trim();
        assertTrue(htmlResponse.startsWith("<body>"));
        assertTrue(htmlResponse.endsWith("</body>"));

        htmlResponse = htmlResponse.substring("<body>".length(), htmlResponse.length() - "</body>".length()).trim();

        if (htmlResponse.isEmpty())
            return Collections.emptyList();

        List<Product> result = new ArrayList<>();
        for (String item : htmlResponse.split("</br>")) {
            String[] nameAndPrice = item.split("\t");
            result.add(new Product(nameAndPrice[0], Integer.parseInt(nameAndPrice[1])));
        }

        return result;
    }

    @Test
    public void testEmpty() {
        List<Product> products = callServletWithValidationAndGetItems();
        assertTrue(products.isEmpty());
    }

    @Test
    public void basicTest() {
        List<Product> products = new ArrayList<>();
        products.add(new Product("a", 1));
        products.add(new Product("b", 2));
        products.add(new Product("c", 3));
        products.forEach(this::addProduct);

        List<Product> returnedProducts = callServletWithValidationAndGetItems();
        assertEquals(products, returnedProducts);
    }

    @Test
    public void randomTest() {
        List<Product> products = new ArrayList<>();

        for (int i = 0; i < 100; i++)
            products.add(new Product(UUID.randomUUID().toString(), ThreadLocalRandom.current().nextInt(0, Integer.MAX_VALUE)));

        products.forEach(this::addProduct);

        List<Product> returnedProducts = callServletWithValidationAndGetItems();
        assertEquals(products, returnedProducts);
    }
}