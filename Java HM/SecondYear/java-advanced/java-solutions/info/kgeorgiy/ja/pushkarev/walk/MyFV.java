package info.kgeorgiy.ja.pushkarev.walk;


import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.FileVisitResult;
import java.nio.file.Path;
import java.nio.file.SimpleFileVisitor;
import java.nio.file.attribute.BasicFileAttributes;


public class MyFV extends SimpleFileVisitor<Path> {

    private final BufferedWriter out;

    private boolean out_is_correct;

    public MyFV(final BufferedWriter out) {
        this.out = out;
        out_is_correct = true;
    }


    @Override
    public FileVisitResult visitFile(final Path file, final BasicFileAttributes attrs) throws IOException{
        try {
            out.write(Hasher.hash(file));
            out.write(' ');
            out.write(file.toString());
            out.newLine();
        } catch (IOException e) {
            System.err.println("Can't can't write to output file");
            out_is_correct = false;
            throw e;
        }
        return FileVisitResult.CONTINUE;
    }

    //out.write(...) can throw IOException
    @Override
    public FileVisitResult visitFileFailed(Path file, IOException exc) throws IOException {
        if(out_is_correct) {
            System.err.println("No such file: \"" + file.toString() + "\"");
            out.write("0000000000000000 ");
            out.write(file.toString());
            out.newLine();
        } else {
            throw exc;
        }
        return FileVisitResult.CONTINUE;
    }
}

