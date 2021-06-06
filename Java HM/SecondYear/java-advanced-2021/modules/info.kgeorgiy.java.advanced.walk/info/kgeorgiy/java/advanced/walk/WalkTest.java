package info.kgeorgiy.java.advanced.walk;

import info.kgeorgiy.java.advanced.base.BaseTest;
import org.junit.Assert;
import org.junit.BeforeClass;
import org.junit.FixMethodOrder;
import org.junit.Test;
import org.junit.runners.MethodSorters;

import java.io.File;
import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.nio.charset.StandardCharsets;
import java.nio.file.*;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.*;
import java.util.stream.Collectors;

/**
 * Tests for easy version
 * of <a href="https://www.kgeorgiy.info/courses/java-advanced/homeworks.html#homework-walk">Walk</a> homework
 * for <a href="https://www.kgeorgiy.info/courses/java-advanced/">Java Advanced</a> course.
 *
 * @author Georgiy Korneev (kgeorgiy@kgeorgiy.info)
 */
@FixMethodOrder(MethodSorters.NAME_ASCENDING)
public class WalkTest extends BaseTest {
    private static final Path DIR = Path.of("__Test__Walk__");
    private static final String ENGLISH_DIGITS = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789";
    private static final SimpleFileVisitor<Path> DELETE = new SimpleFileVisitor<>() {
        @Override
        public FileVisitResult visitFile(final Path file, final BasicFileAttributes attrs) throws IOException {
            Files.delete(file);
            return FileVisitResult.CONTINUE;
        }

        @Override
        public FileVisitResult postVisitDirectory(final Path dir, final IOException exc) throws IOException {
            Files.delete(dir);
            return FileVisitResult.CONTINUE;
        }
    };
    protected static final String ERROR_HASH = "0000000000000000";

    private String alphabet = ENGLISH_DIGITS;
    protected final Random random = new Random(23084701432182342L);

    @BeforeClass
    public static void beforeClass() throws IOException {
        if (Files.exists(DIR)) {
            Files.walkFileTree(DIR, DELETE);
        }
    }

    protected Path getTestDir() {
        return DIR.resolve(testMethodName);
    }

    @Test
    public void test10_oneEmptyFile() throws IOException {
        test(randomFiles(1, 0));
    }

    @Test
    public void test15_tenEmptyFiles() throws IOException {
        test(randomFiles(10, 0));
    }

    @Test
    public void test20_smallRandomFiles() throws IOException {
        test(randomFiles(10, 100));
    }

    @Test
    public void test21_mediumRandomFiles() throws IOException {
        test(randomFiles(10, 100));
    }

    @Test
    public void test22_largeRandomFiles() throws IOException {
        test(randomFiles(10, 1_000_000));
    }

    @Test
    public void test23_veryLargeFile() throws IOException {
        test(randomFiles(1, 10_000_000));
    }

    @Test
    public void test30_missingFiles() throws IOException {
        final Map<String, String> files = randomFiles(3, 0);
        files.put(randomFileName(), ERROR_HASH);
        files.put(randomFileName(), ERROR_HASH);
        files.put(randomFileName(), ERROR_HASH);
        final String upperPath = System.getProperty("os.name").toLowerCase().contains("win")
                ? "//.."
                : "/../...";
        files.put(upperPath, ERROR_HASH);

        test(files);
    }

    @Test
    public void test40_errorReading() throws IOException {
        final Map<String, String> files = randomFiles(3, 0);
        files.put(DIR.toString() + "..", ERROR_HASH);
        files.put(DIR.toString() + "@", ERROR_HASH);
        test(files);
    }

    @Test
    public void test45_partiallyMissingFiles() throws IOException {
        final Map<String, String> files = new LinkedHashMap<>();
        files.put("no-such-file-1", ERROR_HASH);
        files.putAll(randomFiles(10, 100));
        files.put("no-such-file-2", ERROR_HASH);
        files.putAll(randomFiles(10, 100));
        files.put("no-such-file-3", ERROR_HASH);
        test(files);
    }

    @Test
    public void test46_filesAndDirs() throws IOException {
        final Map<String, String> entries = new HashMap<>();
        entries.putAll(randomFiles(10, 100));
        entries.putAll(randomDirs(10));
        test(entries);
    }

    @Test
    public void test50_whitespaceSupport() throws IOException {
        testAlphabet(10, 100, " \u00a0_");
    }

    @Test
    public void test55_chineseSupport() throws IOException {
        testAlphabet(10, 100, "\u8acb\u554f\u4f60\u7684\u7a0b\u5e8f\u652f\u6301\u4e2d\u570b");
    }

    private void testAlphabet(final int n, final int maxL, final String alphabet) throws IOException {
        this.alphabet = alphabet;
        test(randomFiles(n, maxL));
        this.alphabet = ENGLISH_DIGITS;
    }

    @Test
    public void test60_noInput() {
        runRaw(randomFileName(), randomFileName());
    }

    @Test
    public void test61_invalidInput() {
        runRaw("/", randomFileName());
        runRaw("\0*", randomFileName());
    }

    @Test
    public void test62_invalidOutput() throws IOException {
        final String input = createEmptyFile(testMethodName);
        runRaw(input, DIR.toString());
        runRaw(input, "\0*");
        final String file = createEmptyFile(testMethodName);
        runRaw(input, file + File.separator + randomFileName());
    }

    @Test
    public void test63_invalidFiles() throws IOException {
        testAlphabet(1, 10, "\0\\*");
    }

    @Test
    public void test70_singleArgument() throws IOException {
        runRaw(createEmptyFile(testMethodName));
    }

    @Test
    public void test71_noArguments() {
        runRaw();
    }

    @Test
    public void test72_nullArguments() {
        runRaw((String[]) null);
    }

    @Test
    public void test73_firstArgumentNull() {
        runRaw(null, "");
    }

    @Test
    public void test74_secondArgumentNull() throws IOException {
        runRaw(createEmptyFile(testMethodName), null);
    }

    @Test
    public void test75_threeArguments() throws IOException {
        runRaw(createEmptyFile("a"), createEmptyFile("b"), "c");
    }

    protected Map<String, String> randomFiles(final int n, final int maxL) throws IOException {
        return randomFiles(n, maxL, getTestDir());
    }

    protected Map<String, String> randomDirs(final int n) throws IOException {
        final Path dir = getTestDir();
        final Map<String, String> result = new HashMap<>();
        for (int i = 0; i < n; i++) {
            final String name = randomFileName();
                final Path path = dir.resolve(name);
                Files.createDirectories(path);
                result.put(path.toString(), ERROR_HASH);
        }
        return result;
    }

    private static String createEmptyFile(final String name) throws IOException {
        final Path input = DIR.resolve(name);
        Files.write(input, new byte[0]);
        return input.toString();
    }

    protected void test(final Map<String, String> files) {
        test(files.keySet(), files);
    }

    protected void test(final Collection<String> inputs, final Map<String, String> files) {
        final Path inputFile = DIR.resolve(testMethodName + ".in");
        final Path outputFile = DIR.resolve(testMethodName + ".out");
        try {
            Files.writeString(inputFile, generateInput(inputs));
        } catch (final IOException e) {
            throw new AssertionError("Cannot write input file " + inputFile);
        }
        run(inputFile, outputFile);
        try {
            for (final String line : Files.readAllLines(outputFile, StandardCharsets.UTF_8)) {
                final String[] parts = line.split(" ", 2);
                Assert.assertEquals("Invalid line format\n" + line, 2, parts.length);
                Assert.assertTrue("Unexpected file " + parts[1], files.containsKey(parts[1]));
                Assert.assertEquals("Wrong hash", files.remove(parts[1]), parts[0]);
            }
        } catch (final IOException e) {
            throw new AssertionError("Cannot read output file " + outputFile);
        }

        Assert.assertTrue("Some files missing: \n    " + String.join("\n    ", files.keySet()), files.isEmpty());
    }

    private static void run(final Path inputFile, final Path outputFile) {
        runRaw(inputFile.toString(), outputFile.toString());
    }

    private static void runRaw(final String... args) {
        final Method method;
        final Class<?> cut = loadClass();
        try {
            method = cut.getMethod("main", String[].class);
        } catch (final NoSuchMethodException e) {
            throw new AssertionError("Cannot find method main(String[]) of " + cut, e);
        }
        try {
            method.invoke(null, (Object) args);
        } catch (final IllegalAccessException e) {
            throw new AssertionError("Cannot call main(String[]) of " + cut, e);
        } catch (final InvocationTargetException e) {
            throw new AssertionError("Error thrown", e.getCause());
        }
    }

    private static String generateInput(final Collection<String> files) {
        final StringWriter stringWriter = new StringWriter();
        final PrintWriter writer = new PrintWriter(stringWriter);
        files.forEach(writer::println);
        writer.close();
        return stringWriter.toString();
    }

    protected Map<String, String> randomFiles(final int n, final int maxL, final Path dir) throws IOException {
        Files.createDirectories(dir);
        final Map<String, String> result = new HashMap<>();
        for (int i = 0; i < n; i++) {
            final String name = randomFileName();
            try {
                final Path file = dir.resolve(name);
                final byte[] bytes = new byte[random.nextInt(maxL + 1)];
                random.nextBytes(bytes);
                Files.write(file, bytes);
                result.put(file.toString(), hash(bytes));
            } catch (final InvalidPathException ignore) {
                result.put(dir + File.separator + name, ERROR_HASH);
            }
        }
        return result;
    }

    protected String randomFileName() {
        return random.ints(30, 0, alphabet.length())
                .mapToObj(i -> alphabet.substring(i, i + 1))
                .collect(Collectors.joining());
    }
    
    public static long hash(final byte[] bytes, final int size, long start) {
        for (int i = 0; i < size; i++) {
            start = (start << 8) + (bytes[i] & 0xff);
            final long high = start & 0xff00_0000_0000_0000L;
            if (high != 0) {
                start ^= high >> 48;
                start &= ~high;
            }
        }
        return start;
    }

    private static String hash(final byte[] bytes) {
        return String.format("%016x", hash(bytes, bytes.length, 0));
    }
}
