package info.kgeorgiy.java.advanced.crawler;

/**
 * Crawls web sites.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
public interface Crawler extends AutoCloseable {
    /**
     * Downloads web site up to specified depth.
     *
     * @param url start <a href="http://tools.ietf.org/html/rfc3986">URL</a>.
     * @param depth download depth.
     * @return download result.
     */
    Result download(String url, int depth);

    /**
     * Closes this web-crawler, relinquishing any allocated resources.
     */
    @Override
    void close();
}
