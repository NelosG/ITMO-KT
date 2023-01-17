package ru.akirakozov.sd.refactoring.servlet;

import org.mockito.Mock;
import ru.akirakozov.sd.refactoring.dao.ProductDao;
import ru.akirakozov.sd.refactoring.entity.Product;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.UncheckedIOException;
import java.util.List;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.mockito.ArgumentMatchers.anyInt;
import static org.mockito.ArgumentMatchers.anyString;
import static org.mockito.Mockito.doAnswer;
import static org.mockito.Mockito.when;

public abstract class AbstractTest {
    @Mock
    protected ProductDao productDao;

    @Mock
    protected HttpServletRequest request;

    @Mock
    protected HttpServletResponse response;


    protected List<Product> executeSelectAll() {
        return productDao.findAll();
    }

    void addProduct(Product product) {
        productDao.save(product);
    }


    protected String callServletWithValidationAndGetResponce() {
        boolean[] contentTypeSet = new boolean[]{false};
        boolean[] statusSet = new boolean[]{false};

        doAnswer(answer -> {
            String contentType = answer.getArgument(0);
            assertEquals("text/html", contentType);
            contentTypeSet[0] = true;
            return null;
        }).when(response).setContentType(anyString());

        doAnswer(answer -> {
            int statusCode = answer.getArgument(0);
            assertEquals(HttpServletResponse.SC_OK, statusCode);
            statusSet[0] = true;
            return null;
        }).when(response).setStatus(anyInt());

        StringWriter stringWriter = new StringWriter();
        PrintWriter writer = new PrintWriter(stringWriter);
        try {
            when(response.getWriter()).thenReturn(writer);
        } catch (IOException e) {
            throw new UncheckedIOException(e);
        }

        runSupport();

        assertTrue(contentTypeSet[0]);
        assertTrue(statusSet[0]);
        return stringWriter.getBuffer().toString().trim();
    }

    List<Product> callServletWithValidationAndGetItems() {
        return getProductsFromResponce(callServletWithValidationAndGetResponce());
    }

    abstract void runSupport();

    abstract List<Product> getProductsFromResponce(String htmlResponse);
}
