package ru.akirakozov.sd.refactoring.servlet;

import javax.servlet.http.HttpServlet;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;

public abstract class ApplicationServlet extends HttpServlet {
    void setupResponse(String content, HttpServletResponse response) throws IOException {
        response.setContentType("text/html");
        response.setStatus(HttpServletResponse.SC_OK);
        response.getWriter().println(content);
    }

    void unknownCommand(String command, HttpServletResponse response) throws IOException {
        setupResponse("Unknown command: " + command, response);
        response.setStatus(HttpServletResponse.SC_NOT_FOUND);
    }
}