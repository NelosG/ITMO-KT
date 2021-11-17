package info.kgeorgiy.java.advanced.crawler;

import java.io.IOException;
import java.util.*;

/**
 * Crawling result.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public class Result {
    private final List<String> downloaded;
    private final Map<String, IOException> errors;

    /**
     * Creates a new {@code Result}.
     * @param downloaded list of successfully downloaded pages.
     * @param errors pages downloaded with errors.
     */
    public Result(final List<String> downloaded, final Map<String, IOException> errors) {
        this.downloaded = List.copyOf(downloaded);
        this.errors = Map.copyOf(errors);
    }

    /**
     * Returns list of successfully downloaded pages.
     */
    public List<String> getDownloaded() {
        return downloaded;
    }

    /**
     * Returns pages downloaded with errors.
     */
    public Map<String, IOException> getErrors() {
        return errors;
    }
}
