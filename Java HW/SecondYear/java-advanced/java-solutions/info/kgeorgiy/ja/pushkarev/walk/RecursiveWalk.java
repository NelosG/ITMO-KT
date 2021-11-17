package info.kgeorgiy.ja.pushkarev.walk;

import java.io.BufferedWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;

public class RecursiveWalk extends Walk {

    public static void main(String[] args) {
        new RecursiveWalk().start(args);
    }


    //Files.walkFileTree(...) can throw IOException if can't write in out file and WalkerException
    protected void write(Path in, BufferedWriter out) throws WalkerException {
        MyFV fv = new MyFV(out);
        try {
            Files.walkFileTree(in, fv);
        } catch (SecurityException e) {
            throw ex("Can't access starting file: " + e.getMessage(), e);
        }  catch (IOException e) {
            throw ex("Can't can't write to output file", e);
        }
    }
}
