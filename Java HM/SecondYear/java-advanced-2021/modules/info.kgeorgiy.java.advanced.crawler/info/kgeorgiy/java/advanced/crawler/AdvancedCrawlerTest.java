package info.kgeorgiy.java.advanced.crawler;

import org.junit.Assert;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;

/**
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class AdvancedCrawlerTest extends HardCrawlerTest {
    @Test
    public void test20_singleHost() throws IOException {
        test("https://itmo.ru", 2, List.of("itmo.ru"));
    }

    @Test
    public void test21_otherHost() throws IOException {
        test("https://itmo.ru", 10, List.of("www.itmo.ru"));
        test("https://itmo.ru", 10, List.of("itmo.ru.ru"));
    }

    @Test
    public void test22_multiHosts() throws IOException {
        test("http://www.kgeorgiy.info", 10, List.of("www.kgeorgiy.info", "www.facebook.com", "twitter.com"));
    }

    @Test
    public void test23_allHosts() throws IOException {
        test("http://www.kgeorgiy.info", 10, List.of(
                "enterprise.github.com",  "gist.github.com",  "github.community",  "education.github.com",
                "github.blog",  "docs.github.com",  "creativecommons.org",  "business.twitter.com",
                "support.github.com",  "www.ifmo.ru",  "help.twitter.com",  "kgeorgiy.info",
                "support.twitter.com",  "lab.github.com",  "gkorneev.moikrug.ru",  "www.kgeorgiy.info",
                "vk.com",  "kgeorgiy.livejournal.com",  "twitter.com",  "www.facebook.com",  "services.github.com",
                "opensource.guide",  "github.com",  "validator.w3.org",  "www.linkedin.com",  "keybase.io",
                "stars.github.com",  "legal.twitter.com",  "gg.gg",  "goo.gl",  "www.githubstatus.com"));
    }

    @Test
    public void test24_megaHosts() throws IOException {
        test("http://www.kgeorgiy.info", 10, Collections.nCopies(1 << 27, "www.kgeorgiy.info"));
    }

    private static void test(final String start, final int depth, final List<String> hosts) throws IOException {
        final ReplayDownloader downloader = new ReplayDownloader(start, 0, 0);
        final Set<String> set = hosts.stream().collect(Collectors.toUnmodifiableSet());
        final Predicate<String> predicate = url -> {
            try {
                return set.contains(URLUtils.getHost(url));
            } catch (final MalformedURLException e) {
                return false;
            }
        };
        checkResult(
                getPages(downloader, start, depth, predicate),
                download(downloader, start, depth, hosts, predicate)
        );
    }

    private static Result download(final Downloader downloader, final String url, final int depth, final List<String> hosts, final Predicate<String> allowed) {
        final CheckingDownloader checkingDownloader = new CheckingDownloader(downloader, Integer.MAX_VALUE, Integer.MAX_VALUE, Integer.MAX_VALUE) {
            @Override
            public Document download(final String url) throws IOException {
                Assert.assertTrue(allowed.test(url));
                return super.download(url);
            }
        };
        try (final AdvancedCrawler crawler = createInstance(checkingDownloader, Integer.MAX_VALUE, Integer.MAX_VALUE, Integer.MAX_VALUE, AdvancedCrawler.class)) {
            final Result result = crawler.download(url, depth, hosts);
            Assert.assertNull(checkingDownloader.getError(), checkingDownloader.getError());
            return result;
        }
    }

    private static Result getPages(final ReplayDownloader downloader, final String url, final int depth, final Predicate<String> predicate) {
        if (!predicate.test(url)) {
            return new Result(List.of(), Map.of());
        }

        final Map<String, ReplayDownloader.Page> level = new HashMap<>(Map.of(url, downloader.getPage(url)));
        for (int i = 1; i < depth; i++) {
            final Map<String, ReplayDownloader.Page> next = new HashMap<>();
            level.values().stream()
                    .map(p -> p.links)
                    .filter(Objects::nonNull)
                    .flatMap(Collection::stream)
                    .filter(predicate)
                    .forEach(link -> next.put(link, downloader.getPage(link)));
            level.putAll(next);
        }
        return ReplayDownloader.expected(level);
    }
}
