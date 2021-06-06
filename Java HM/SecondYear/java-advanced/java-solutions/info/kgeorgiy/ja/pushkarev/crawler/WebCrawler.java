package info.kgeorgiy.ja.pushkarev.crawler;

import info.kgeorgiy.java.advanced.crawler.AdvancedCrawler;
import info.kgeorgiy.java.advanced.crawler.CachingDownloader;
import info.kgeorgiy.java.advanced.crawler.Crawler;
import info.kgeorgiy.java.advanced.crawler.Document;
import info.kgeorgiy.java.advanced.crawler.Downloader;
import info.kgeorgiy.java.advanced.crawler.Result;
import info.kgeorgiy.java.advanced.crawler.URLUtils;

import java.io.IOException;
import java.net.MalformedURLException;
import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Objects;
import java.util.Queue;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ConcurrentLinkedQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Phaser;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import java.util.stream.Collectors;

public class WebCrawler implements AdvancedCrawler {

    private final static int AWAIT_TIME = 3;
    private final static int SEMAPHORE_AWAIT_TIME = 100;

    private final Downloader downloader;
    private final ExecutorService downloaders;
    private final ExecutorService extractors;
    private final int perHost;
    private final Phaser phaser = new Phaser(1);

    private final Map<String, Semaphore> hostLimiters = new ConcurrentHashMap<>();
    private Map<String, IOException> failed = new ConcurrentHashMap<>();
    private final Set<String> downloaded = ConcurrentHashMap.newKeySet();
    private final Queue<String> extracted = new ConcurrentLinkedQueue<>();

    private Control control = null;
    private boolean closed = false;

    public WebCrawler(final Downloader downloader, final int downloaders, final int extractors, final int perHost) {
        this.downloader = downloader;
        this.downloaders = Executors.newFixedThreadPool(downloaders);
        this.extractors = Executors.newFixedThreadPool(extractors);
        this.perHost = perHost;
    }

    public static void main(final String[] args) {
        Objects.requireNonNull(args);
        if (args.length == 0) {
            System.err.println("Usage: WebCrawler url [depth [downloaders [extractors [perHost]]]].");
            return;
        }
        try {
            final String url = Objects.requireNonNull(args[0]);
            final int depth = getArgs(args, 1, "depth");
            final int downloaders = getArgs(args, 2, "downloaders");
            final int extractors = getArgs(args, 3, "extractors");
            final int perHost = getArgs(args, 4, "perHost");
            try (final Crawler wc = new WebCrawler(new CachingDownloader(), downloaders, extractors, perHost)) {
                wc.download(url, depth);
            } catch (final IOException e) {
                System.err.println("An error occurred while initializing downloader.");
            }
        } catch (final IllegalArgumentException e) {
            System.err.println("Invalid argument: " + e.getMessage());
        }
    }

    private static int getArgs(final String[] args, final int i, final String what) {
        if (args.length <= i) {
            return 1;
        }
        Objects.requireNonNull(args[i]);
        final int res = Integer.parseInt(args[i]);
        if (res < 1) {
            throw new IllegalArgumentException("args[" + i + "](" + what + ") must be > 0.");
        }
        return res;
    }

    @Override
    public Result download(final String url, final int depth) {
        checkClosed();
        control = new ControlSet(ConcurrentHashMap.newKeySet());
        return downloadImpl(url, depth);
    }

    @Override
    public Result download(String url, int depth, List<String> hosts) {
        checkClosed();
        control = new ControlSetWithHostFilter(ConcurrentHashMap.newKeySet(), hosts);
        return downloadImpl(url, depth);
    }

    @Override
    public void close() {
        if(!closed) {
            downloaders.shutdown();
            extractors.shutdown();
            while (true) {
                try {
                    if (downloaders.awaitTermination(AWAIT_TIME, TimeUnit.SECONDS) &&
                            extractors.awaitTermination(AWAIT_TIME, TimeUnit.SECONDS)) {
                        break;
                    }
                } catch (final InterruptedException e) {
                    System.err.println(e.getMessage());
                }
                downloaders.shutdownNow();
                extractors.shutdownNow();
            }
            clear();
            closed = true;
        }
    }

    private void checkClosed() {
        if (closed) {
            throw new IllegalStateException("WebCrawler closed.");
        }
    }

    private Result downloadImpl(final String url, final int depth) {
        control.check(url);
        extracted.add(url);
        // :NOTE: this abstraction is unnecessarily complex

        //FIXED
        startDownload(0, depth);
        phaser.arriveAndAwaitAdvance();
        Result res = new Result(List.copyOf(downloaded), failed);
        clear();
        return res;
    }

    private void clear() {
        hostLimiters.clear();
        extracted.clear();
        downloaded.clear();

        control = null;
        failed = new ConcurrentHashMap<>();
    }

    private synchronized void startDownload(final int now, final int depth) {
        while (!extracted.isEmpty()) {
            downloadAndExtract(extracted.poll(), now, depth);
        }
    }

    private synchronized void addExtracted(final Collection<String> c) {
        extracted.addAll(c.stream().filter(control::check).collect(Collectors.toList()));
    }

    private void downloadAndExtract(final String url, int now, int depth) {
        try {
            final Semaphore hostLimiter = hostLimiters.computeIfAbsent(URLUtils.getHost(url), s -> new Semaphore(perHost));
            phaser.register();
            downloaders.submit(() -> {
                boolean flag = true; // :NOTE-2: the name is too abstract
                try {
                    // :NOTE: why doesn't it interfere with other tasks
                    // that access another URL that does not have its perHost exceeded
                    // 100 requests to kgeorgiy.info, 10 request to asta.ru
                    // 10 downloaders
                    // 5 perHost
                    // 5 downloaders download kgeorgiy.info, other 5 wait a permit for kgeorgiy.info
                    // but it would be better if 5 of them downloaded kgeorgiy.info and 5 of them asta.ru

                    //FIXED

                    // :NOTE-2: does not fix unfortunately, you still actively poll
                    if (!hostLimiter.tryAcquire(SEMAPHORE_AWAIT_TIME, TimeUnit.MILLISECONDS)) {
                        downloadAndExtract(url, now, depth);
                        flag = false;
                        return;
                    }
                    final Document document = downloader.download(url);
                    downloaded.add(url);
                    extract(document, url, now + 1, depth);
                } catch (final InterruptedException e) {
                    System.err.println("Downloading of \"" + url + "\" stopped: " + e.getMessage());
                } catch (final IOException e) {
                    failed.put(url, e);
                } finally {
                    if (flag) {
                        hostLimiter.release();
                    }
                    phaser.arriveAndDeregister();
                }
            });
        } catch (final MalformedURLException e) {
            failed.put(url, e);
        }
    }

    private void extract(final Document document, final String url, final int now, final int depth) {
        if (now < depth) {
            phaser.register();
            extractors.submit(() -> {
                try {
                    addExtracted(document.extractLinks());
                    startDownload(now, depth);
                } catch (final IOException e) {
                    System.err.println("Can't extract links from \"" + url + "\": " + e.getMessage());
                } finally {
                    phaser.arriveAndDeregister();
                }
            });
        }
    }
}
