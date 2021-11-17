package info.kgeorgiy.ja.pushkarev.implementor;

import info.kgeorgiy.java.advanced.implementor.ImplerException;
import info.kgeorgiy.java.advanced.implementor.JarImpler;

import javax.tools.JavaCompiler;
import javax.tools.ToolProvider;
import java.io.File;
import java.io.IOException;
import java.io.Writer;
import java.lang.reflect.Executable;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.URISyntaxException;
import java.nio.file.FileVisitResult;
import java.nio.file.Files;
import java.nio.file.InvalidPathException;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Objects;
import java.util.Set;
import java.util.function.Predicate;
import java.util.jar.Attributes;
import java.util.jar.JarOutputStream;
import java.util.jar.Manifest;
import java.util.stream.Collectors;
import java.util.zip.ZipEntry;


/**
 * Implementation {@code class} for {@link JarImpler} {@code Interface}.
 * <p>
 * Generates {@code class} that implements given {@link java.lang.Class}.
 * </p>
 *
 * @author NelosG
 * @version 1.0
 */
public class Implementor implements JarImpler {

    /**
     * Name of the implementation {@code class}.
     */
    private String className;

    /**
     * Creates new instance of {@code Implementor}.
     */
    public Implementor() {
    }

    /**
     * Creates {@link java.lang.String} with <em>default value</em> returned from some {@link java.lang.reflect.Method}.
     *
     * @param token {@link java.lang.Class} returned by some {@link java.lang.reflect.Method}.
     * @return {@link java.lang.String} with <em>default value</em>.
     */
    private static String getDefaultValue(final Class<?> token) {
        if (token.equals(boolean.class)) {
            return " false";
        } else if (token.equals(void.class)) {
            return "";
        } else if (token.isPrimitive()) {
            return " 0";
        }
        return " null";
    }


    /**
     * Creates {@link java.lang.String} with <em>heading</em> of {@code class}
     * that will implement given {@link java.lang.Class}.
     *
     * @param token base {@link java.lang.Class}.
     * @return {@link java.lang.String} with <em>heading</em> of implementing {@code class}.
     */
    private String getClassHeading(final Class<?> token) {
        final String pack = token.getPackageName();
        return String.format(
                "%s%n%npublic class %s %s %s {%n",
                pack.isEmpty() ? "" : "package " + pack + ";",
                className,
                token.isInterface() ? "implements" : "extends",
                token.getCanonicalName()
        );
    }

    /**
     * Creates {@link java.lang.String} with <em>names</em> of {@link java.lang.reflect.Parameter parameters}
     * of {@link java.lang.reflect.Method} or {@link java.lang.reflect.Constructor} and theirs <em>types</em>.
     *
     * @param exec       {@link java.lang.reflect.Method} or {@link java.lang.reflect.Constructor}.
     * @param typeNeeded if {@code true} add type name of each {@link java.lang.reflect.Parameter}
     *                   before it.
     * @return {@link java.lang.String} with {@link java.lang.reflect.Parameter parameters} <em>names</em>
     * and theirs <em>types</em>.
     */
    private static String getParams(final Executable exec, final boolean typeNeeded) {
        return Arrays.stream(exec.getParameters())
                .map(param -> (typeNeeded ? param.getType().getCanonicalName() + " " : "") + param.getName())
                .collect(Collectors.joining(", ", "(", ")"));
    }

    /**
     * Creates {@link java.lang.String} with {@link java.lang.Exception Exceptions}
     * that the given {@link java.lang.reflect.Executable} throws.
     *
     * @param exec base {@link java.lang.reflect.Executable}.
     * @return {@link java.lang.String} with list of {@link java.lang.Exception Exceptions}.
     */
    private static String getExceptions(final Executable exec) {
        final Class<?>[] exceptions = exec.getExceptionTypes();
        return exceptions.length == 0 ? "" : " throws " + Arrays.stream(exceptions)
                .map(Class::getCanonicalName)
                .collect(Collectors.joining(", "));
    }

    /**
     * Filters {@link java.lang.reflect.Method methods} by {@link java.util.function.Predicate}
     * and puts them into a {@code storage}.
     *
     * @param methods   {@code Array} of {@link java.lang.reflect.Method}.
     * @param predicate {@link java.util.function.Predicate} to filter {@link java.lang.reflect.Method methods}.
     * @param storage   filtered {@link java.lang.reflect.Method} {@link java.util.Set storage}.
     */
    private static void getMethods(final Method[] methods,
                                   final Predicate<Integer> predicate,
                                   final Set<MethodContainer> storage) {
        Arrays.stream(methods)
                .filter(method -> predicate.test(method.getModifiers()))
                .map(MethodContainer::new)
                .forEach(storage::add);
    }


    /**
     * Creates {@link java.lang.String} with <em>implementation</em> of {@link java.lang.reflect.Method}
     * of given {@link java.lang.Class}.
     *
     * @param token base {@link java.lang.Class} to implement {@link java.lang.reflect.Method methods}.
     * @return {@link java.lang.String} with created <em>implementation</em>.
     */
    private static String implementMethods(final Class<?> token) {
        final Set<MethodContainer> abstractMethods = new HashSet<>();
        final Set<MethodContainer> finalMethods = new HashSet<>();

        getMethods(token.getMethods(), Modifier::isAbstract, abstractMethods);
        getMethods(token.getMethods(), Modifier::isFinal, finalMethods);
        Class<?> superToken = token;
        while (superToken != null) {
            getMethods(superToken.getDeclaredMethods(), Modifier::isAbstract, abstractMethods);
            getMethods(superToken.getDeclaredMethods(), Modifier::isFinal, finalMethods);
            superToken = superToken.getSuperclass();
        }

        return abstractMethods.stream()
                .filter(Predicate.not(finalMethods::contains))
                .map(MethodContainer::get)
                .map(method -> String.format("%n    @Override%n    %s%s%s%s {%n        return%s;%n    }%n",
                        getModifiers(method),
                        method.getReturnType().getCanonicalName() + " " + method.getName(),
                        getParams(method, true),
                        getExceptions(method),
                        getDefaultValue(method.getReturnType())
                ))
                .collect(Collectors.joining());
    }

    /**
     * Creates {@link java.lang.String} with <em>implementation</em> of {@link java.lang.reflect.Constructor}
     * of given {@link java.lang.Class}.
     *
     * @param token base {@link java.lang.Class} to implement {@link java.lang.reflect.Constructor}.
     * @return {@link java.lang.String} with created <em>implementation</em>.
     * @throws ImplerException if {@link java.lang.Class}
     *                         doesn't have any non-private {@link java.lang.reflect.Constructor constructors}.
     */
    private String implementConstructors(final Class<?> token) throws ImplerException {
        final String str = Arrays.stream(token.getDeclaredConstructors())
                .filter(constructor -> !Modifier.isPrivate(constructor.getModifiers()))
                .map(s -> String.format("%n    %s%s%s%s {%n        super%s;%n    }%n",
                        getModifiers(s),
                        className,
                        getParams(s, true),
                        getExceptions(s),
                        getParams(s, false)
                ))
                .collect(Collectors.joining());
        if (str.isEmpty()) {
            throw new ImplerException("No non-private constructors in the class");
        }
        return str;
    }

    /**
     * Creates {@link java.lang.String} with {@link java.lang.reflect.Modifier modifiers}
     * of given {@link java.lang.reflect.Executable}.
     *
     * @param exec {@link java.lang.reflect.Executable} to take {@link java.lang.reflect.Modifier modifiers}.
     * @return {@link java.lang.String} with {@link java.lang.reflect.Modifier modifiers}.
     */
    private static String getModifiers(final Executable exec) {
        final int mods = exec.getModifiers() & ~Modifier.ABSTRACT & ~Modifier.TRANSIENT;
        return mods > 0 ? Modifier.toString(mods) + " " : "";
    }

    /**
     * Creates {@link java.lang.String} with {@link java.nio.file.Path} to directory with compiled
     * <em><strong>.class</strong></em> files of given {@link java.lang.Class} token.
     *
     * @param token {@link java.lang.Class} to find <em><strong>.class</strong></em> files.
     * @return {@link java.lang.String} with {@link java.nio.file.Path} to directory with compiled
     * <em><strong>.class</strong></em> files.
     * @throws ImplerException if can't find {@link java.nio.file.Path}.
     */
    private static String getClassPath(final Class<?> token) throws ImplerException {
        try {
            return Path.of(token.getProtectionDomain().getCodeSource().getLocation().toURI()).toString();
        } catch (final URISyntaxException e) {
            throw new ImplerException(e);
        }
    }

    /**
     * Cleans up <var>temp</var> directory and delete it.
     *
     * @param root start directory.
     * @throws ImplerException if can't clear directory and subdirectories.
     */
    public static void cleanDirectory(final Path root) throws ImplerException {
        try {
            Files.walkFileTree(root, new DeletingFileVisitor());
        } catch (final IOException e) {
            throw new ImplerException("Could not clean temporary directory at: " + root);
        }
    }

    /**
     * Console interface for {@code Implementor}.
     * <p>
     * <strong>Correct usage:</strong>
     * <blockquote><ul>
     * <li><strong>{@code <InterfaceOrClassName> <DirectoryName>}</strong> creates <em>implementation</em>
     * of given <var>token</var> and places it in given <var>root</var> directory.</li>
     * <li><strong>{@code -jar <InterfaceOrClassName> <PathToOutputJarFile>}</strong> creates <em>implementation</em>
     * of given <var>token</var> and places it in given <em><strong>.jar</strong></em> file.</li>
     * </ul></blockquote>
     *
     * @param args arguments to run {@code Implementor}.
     */
    public static void main(final String[] args) {
        if (args == null || (args.length != 2 && args.length != 3)) {
            System.err.println("Two or three arguments expected");
            printUsage();
            return;
        }
        final JarImpler implementor = new Implementor();
        try {
            if (args.length == 2) {
                implementor.implement(Class.forName(args[0]), Paths.get(args[1]));
            } else if (args[0].equals("-jar")) {
                implementor.implementJar(Class.forName(args[1]), Paths.get(args[2]));
            } else {
                System.err.println("Incorrect arguments.");
                printUsage();
            }
        } catch (final InvalidPathException e) {
            System.err.println("Incorrect path to root: " + e.getMessage());
        } catch (final ClassNotFoundException e) {
            System.err.println("Incorrect class name: " + e.getMessage());
        } catch (final ImplerException e) {
            System.err.println("An ERROR occurred during implementation: " + e.getMessage());
        }
    }

    /**
     * Prints instructions for entering arguments on standard output.
     */
    private static void printUsage() {
        System.out.println("Correct usage:");
        System.out.println("    <InterfaceOrClassName> <DirectoryName>");
        System.out.println("    -jar <InterfaceOrClassName> <PathToOutputJarFile>");
    }

    /**
     * Converts {@link java.lang.String} to <strong>Unicode</strong>.
     *
     * @param str input {@link java.lang.String}.
     * @return {@link java.lang.String} <strong>Unicode</strong> representation of {@code str}.
     */
    private String toUnicode(final String str) {
        final StringBuilder sb = new StringBuilder();
        for (final char c : str.toCharArray()) {
            sb.append(c < 128 ? c : String.format("\\u%04X", (int) c));
        }
        return sb.toString();
    }

    /**
     * Resolves {@code root} {@link java.nio.file.Path} to <em><strong>.java</strong></em> file.
     *
     * @param root  {@link java.nio.file.Path} to {@code root} file.
     * @param token {@link java.lang.Class} token to get {@link java.nio.file.Path} for.
     * @return {@link java.nio.file.Path} from {@code root} directory to file.
     */
    private Path getFilePath(final Path root, final Class<?> token) {
        return root.resolve(Path.of(getPackageDir(token, File.separator), className + ".java"));
    }

    /**
     * Getter to get <em>path</em> inside package.
     *
     * @param token     {@link Class} token to get package <em>path</em> for.
     * @param separator Separator to create <em>path</em>.
     * @return {@link java.lang.String} with <em>path</em> from upper package directory to files package.
     */
    private String getPackageDir(final Class<?> token, final String separator) {
        return token.getPackageName().replace(".", separator);
    }

    /**
     * Creates parent directories of specified file or directory.
     *
     * @param root {@link java.nio.file.Path} to specified file or directory.
     * @return {@link java.nio.file.Path} to created directory.
     * @throws ImplerException if catch {@link java.io.IOException} when creating parent directories.
     */
    private Path createDirectories(final Path root) throws ImplerException {
        try {
            return Files.createDirectories(root.getParent());
        } catch (final IOException e) {
            throw new ImplerException("Can't create directories for output file", e);
        }
    }

    /**
     * Create <em>implementation</em> {@code class} that implement base class.
     *
     * @param token {@link java.lang.Class} token to create <em>implementation</em> for.
     * @param root  {@link java.nio.file.Path} to <em>root</em> directory.
     * @throws ImplerException if <em>implementation</em> for given {@link java.lang.Class}
     *                         cannot be generated for one of such reasons:
     *                         <ul>
     *                         <li> Some arguments are {@code null}</li>
     *                         <li> Given {@link java.lang.Class} is {@code primitive}
     *                         or <strong>{@code array}</strong>. </li>
     *                         <li> Given {@link java.lang.Class} is {@code final class} or {@link Enum}. </li>
     *                         <li> Given {@link java.lang.Class} isn't an {@code Interface}
     *                         and contains only private {@link java.lang.reflect.Constructor constructors}. </li>
     *                         <li> The problems with I/O occurred during <em>implementation</em>. </li>
     *                         </ul>
     */
    @Override
    public void implement(final Class<?> token, final Path root) throws ImplerException {
        if (token == null || root == null) {
            throw new NullPointerException("Not-null arguments expected.");
        }
        if (token.isPrimitive()
                || token.isArray()
                || Modifier.isFinal(token.getModifiers())
                || Modifier.isPrivate(token.getModifiers())
                || token == Enum.class
                || token.isEnum()
        ) {
            throw new ImplerException("Incorrect class token: token isn't public class or interface.");
        }

        className = token.getSimpleName() + "Impl";
        final Path file = getFilePath(root, token);
        createDirectories(file);

        // :NOTE: Кодировка по-умолчанию
        try (final Writer writer = Files.newBufferedWriter(file)) {
            writer.write(toUnicode(String.format(
                    "%s%s%s}%n",
                    getClassHeading(token),
                    token.isInterface() ? "" : implementConstructors(token),
                    implementMethods(token)
            )));
        } catch (final IOException e) {
            throw new ImplerException("Unable to write to output file.", e);
        }
    }

    /**
     * Creates <em><strong>.jar</strong></em> file with <em><strong>.class</strong></em> file
     * of implemented {@code class}.
     *
     * @param token {@link java.lang.Class} token to create implementation for.
     * @param root  {@link java.nio.file.Path} where will be created <em><strong>.jar</strong></em> file.
     * @throws ImplerException if <em>implementation <strong>.jar</strong></em> file for given {@link java.lang.Class}
     *                         cannot be generated for one of such reasons:
     *                         <ul>
     *                         <li> {@code root} are {@code null}</li>
     *                         <li> The problems with I/O occurred during implementation. </li>
     *                         </ul>
     */
    @Override
    public void implementJar(final Class<?> token, final Path root) throws ImplerException {
        if (root == null) {
            throw new NullPointerException("Not-null root path expected.");
        }

        final Path temp;
        try {
            temp = Files.createTempDirectory(createDirectories(root), "For-compile");
        } catch (final IOException e) {
            throw new ImplerException("Can't create temporary directory.");
        }


        try {
            implement(token, temp);
            compile(token, temp);
            buildJar(token, root, temp);
        } finally {
            cleanDirectory(temp);
        }
    }

    /**
     * Compiles generated <em><strong>.java</strong></em> file in {@code temp} directory.
     *
     * @param token {@link java.lang.Class} token for compile.
     * @param temp  temporary directory.
     * @throws ImplerException if can't compile file.
     */
    private void compile(final Class<?> token, final Path temp) throws ImplerException {
        final JavaCompiler compiler = ToolProvider.getSystemJavaCompiler();
        if (compiler == null ||
                compiler.run(null, null, null,
                        "-cp",
                        temp + File.pathSeparator + getClassPath(token),
                        getFilePath(temp, token).toString()) != 0
        ) {
            throw new ImplerException("Failed to compile class.");
        }
    }

    /**
     * Generates <em><strong>.jar</strong></em> file in specified {@link java.nio.file.Path},
     * from <em><strong>.class</strong></em> files in temporary directory.
     *
     * @param token   {@link java.lang.Class} token to find <em><strong>.class</strong></em> files
     *                of this {@code class}.
     * @param jarFile {@link java.nio.file.Path} for <em><strong>.jar</strong></em> file.
     * @param temp    temporary directory.
     * @throws ImplerException if can't create or correctly write in <em><strong>.jar</strong></em> file.
     */
    private void buildJar(final Class<?> token, final Path jarFile, final Path temp) throws ImplerException {
        final Manifest manifest = new Manifest();
        manifest.getMainAttributes().put(Attributes.Name.MANIFEST_VERSION, "1.0");
        manifest.getMainAttributes().put(Attributes.Name.IMPLEMENTATION_VENDOR, "NelosG");
        try (final JarOutputStream out = new JarOutputStream(Files.newOutputStream(jarFile), manifest)) {
            final String name = getPackageDir(token, "/") + "/" + className + ".class";
            out.putNextEntry(new ZipEntry(name));
            Files.copy(temp.resolve(name), out);
        } catch (final IOException e) {
            throw new ImplerException("Error when working with jar file.", e);
        }
    }

    /**
     * Static {@code class} used for recursive deleting of folders.
     */
    private static class DeletingFileVisitor extends SimpleFileVisitor<Path> {
        /**
         * Default {@link java.lang.reflect.Constructor}. Creates a new instance of {@code DeletingFileVisitor}.
         */
        public DeletingFileVisitor() {
        }

        /**
         * Deletes the specified <var>file</var>.
         *
         * @throws java.io.IOException if the method fails to delete the specified <var>file</var>.
         */
        @Override
        public FileVisitResult visitFile(final Path file, final BasicFileAttributes attrs) throws IOException {
            Files.delete(file);
            return FileVisitResult.CONTINUE;
        }

        /**
         * Deletes the specified <var>dir</var>.
         *
         * @throws java.io.IOException if the method fails to delete the specified <var>dir</var>, or
         *                             any <var>file</var> under it.
         */
        @Override
        public FileVisitResult postVisitDirectory(final Path dir, final IOException e) throws IOException {
            if (e == null) {
                Files.delete(dir);
                return FileVisitResult.CONTINUE;
            } else {
                throw e;
            }
        }
    }

    /**
     * Static {@code class} used for correct representing {@link Method}.
     */
    private static class MethodContainer {

        /**
         * Wrapped instance of {@link java.lang.reflect.Method}.
         */
        private final Method method;

        /**
         * Create new <strong>MethodContainer</strong> instant
         * containing {@link java.lang.reflect.Method}.
         *
         * @param method some {@link java.lang.reflect.Method}.
         */
        public MethodContainer(final Method method) {
            this.method = method;
        }

        /**
         * Compares given object with this instant.
         *
         * @param obj {@link java.lang.Object}.
         * @return <strong>{@code true}</strong> if there are two identical methods defined in the {@code class} for implementation
         * or its {@code superclasses}, {@code false} - otherwise.
         */
        @Override
        public boolean equals(final Object obj) {
            if (obj instanceof MethodContainer) {
                final MethodContainer other = (MethodContainer) obj;
                return Arrays.equals(method.getParameterTypes(), other.method.getParameterTypes())
                        && method.getName().equals(other.method.getName());
            }
            return false;
        }

        /**
         * Calculates {@code hashcode} for this wrapper using hashes of <em>name</em>,
         * return <em>type</em> and {@link java.lang.reflect.Parameter parameters}
         * <em>types</em> of its {@link #method}.
         *
         * @return {@code hashcode} for this wrapper.
         */
        @Override
        public int hashCode() {
            return Objects.hash(method.getName(), Arrays.hashCode(method.getParameterTypes()));
        }

        /**Getter for {@link java.lang.reflect.Method} inside <strong>MethodContainer</strong>.
         *
         * @return {@link #method}
         */
        public Method get() {
            return method;
        }
    }
}
