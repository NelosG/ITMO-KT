package info.kgeorgiy.ja.pushkarev.walk;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.*;

public class Walk {

    public static void main(String[] args) {
        new Walk().start(args);
    }

    public void start(String[] args) {
        if (args == null || args.length != 2 || args[0] == null || args[1] == null) {
            System.err.println("Usage: java <\"Walk\" or \"RecursiveWalk\" > <input file> <output file>");
            return;
        }
        Path outputFile;
        Path parent;
        Path inputFile;
        try {

            inputFile = Paths.get(args[0]);
            try {
                outputFile = Paths.get(args[1]);

            } catch (InvalidPathException e) {
                System.err.println("Invalid output file \"" + args[1] + "\"");
                return;
            }

        } catch (InvalidPathException e) {
            System.err.println("Invalid input file \"" + args[0] + "\"");
            return;
        }

        parent = outputFile.getParent();

        try {

            if (parent != null) {
                Files.createDirectories(parent);
            }

        } catch (FileAlreadyExistsException e) {
            System.err.println("Exists file with same name as directory in prefix path of the output file \""
                    + args[1] + "\"");
            return;
        } catch (IOException e) {
            System.err.println("IOException while creating parent directories to the output file \"" + args[1] + "\"");
            return;
        } catch (SecurityException e) {
            System.err.println("Not enough rights parent directories to the output file \"" + args[1] + "\"");
            return;
        }

        try {

            walk(inputFile, outputFile);

        } catch (WalkerException e) {
            System.err.println(e.getMessage());
        }
    }

    protected void walk(Path inputFile, Path outputFile) throws WalkerException {
        try (BufferedReader in = Files.newBufferedReader(inputFile)) {
            try (BufferedWriter out = Files.newBufferedWriter(outputFile)) {
                String in_path;
                long line = -1;
                try {
                    while ((in_path = in.readLine()) != null) {
                        line++;
                        try {
                            write(Path.of(in_path), out);
                        } catch (InvalidPathException e) {
                            System.err.println("Invalid path specified in input file: " + in_path);
                            out.write("0000000000000000 ");
                            out.write(in_path);
                            out.newLine();
                        }
                    }
                } catch (IOException e) {
                    throw ex("Error while reading input file in line number: " + line + " : "
                            + e.getMessage(), e);
                }
            } catch (IOException e) {
                throw ex("Can't open output file for write", e);
            } catch (SecurityException e) {
                throw ex("Can't open output file for write: not enough rights", e);
            }

        } catch (NoSuchFileException e) {
            throw ex("Can't open input file for read: file doesn't exist", e);
        } catch (IOException e) {
            throw ex("Can't open input file for read", e);
        } catch (SecurityException e) {
            throw ex("Can't open input file for read: not enough rights", e);
        }
    }


    protected void write(Path in, BufferedWriter out) throws WalkerException {
        try {
            out.write(Hasher.hash(in));
            out.write(' ');
            out.write(in.toString());
            out.newLine();
        } catch (IOException e) {
            throw ex("Can't can't write to output file", e);
        }
    }


    protected WalkerException ex(String message, Throwable e) {
        return new WalkerException(message, e);
    }

}
