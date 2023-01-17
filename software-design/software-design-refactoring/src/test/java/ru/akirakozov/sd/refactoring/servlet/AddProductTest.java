package ru.akirakozov.sd.refactoring.servlet;

import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.junit.MockitoJUnitRunner;
import ru.akirakozov.sd.refactoring.dao.ProductDao;
import ru.akirakozov.sd.refactoring.entity.Product;

import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.UUID;
import java.util.concurrent.ThreadLocalRandom;

import static org.junit.Assert.assertEquals;
import static org.mockito.Mockito.when;

@RunWith(MockitoJUnitRunner.class)
public class AddProductTest extends AbstractTest {
    private List<Product> callServletWithValidation(Product product) {
        when(request.getParameter("name")).thenReturn(product.getName());
        when(request.getParameter("price")).thenReturn(String.valueOf(product.getPrice()));
        return callServletWithValidationAndGetItems();
    }

    @Override
    void runSupport() {
        try {
            new AddProductServlet(new ProductDao()).doGet(request, response);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    @Override
    List<Product> getProductsFromResponce(String htmlResponse) {
        assertEquals("OK", htmlResponse);
        return executeSelectAll();
    }

    @Test
    public void addOne() {
        Product product = new Product("product1", 123);
        List<Product> products = callServletWithValidation(product);
        assertEquals(1, products.size());
        assertEquals(product, products.get(0));
    }

    @Test
    public void addMany() {
        List<Product> products = new ArrayList<>();
        products.add(new Product("abacaba", 52654));
        products.add(new Product("fdsergqe", 5494));
        products.add(new Product("asdfq", 69871));
        products.add(new Product("asdfwegebvw", 26489));

        for (int i = 0; i < products.size(); i++) {
            List<Product> dbProducts = callServletWithValidation(products.get(i));

            assertEquals(i + 1, dbProducts.size());
            assertEquals(products.subList(0, i + 1), dbProducts);
        }
    }

    @Test
    public void randomTest() {
        Set<Product> products = new HashSet<>();

        for (int i = 0; i < 100; i++)
            products.add(new Product(
                    UUID.randomUUID().toString(),
                    ThreadLocalRandom.current().nextInt(0, Integer.MAX_VALUE)
            ));

        products.forEach(this::callServletWithValidation);

        Set<Product> dbProducts = new HashSet<>(callServletWithValidationAndGetItems());
        assertEquals(products, dbProducts);
    }
}