package info.kgeorgiy.ja.pushkarev.walk;

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.nio.file.Files;
import java.nio.file.NoSuchFileException;
import java.nio.file.Path;

public class Hasher {

    //noexcept method
    public static String hash(Path File) {
        try {
            return String.format("%016x", hash_cal(File));
        } catch (WalkerException e) {
            System.err.println(e.getMessage());
        }
        return "0000000000000000";
    }


    private static long hash_cal(final Path path) throws WalkerException {
        try (final InputStream in = new BufferedInputStream(Files.newInputStream(path))) {
            long hash = 0;
            byte[] bytes = new byte[1000];
            int col = in.read(bytes);
            while (col != -1) {
                for (int i = 0; i < col; i++) {
                    hash = (hash << 8) + (bytes[i] & 0xff);
                    final long high = hash & 0xff00_0000_0000_0000L;
                    if (high != 0) {
                        hash ^= high >> 48;
                        hash &= ~high;
                    }
                }
                col = in.read(bytes);
            }
            return hash;
        } catch (final NoSuchFileException e) {
            throw new WalkerException("File \"" + path + "\" doesn't exist", e);
        } catch (final SecurityException e) {
            throw new WalkerException("Unable to access file \"" + path + "\"", e);
        } catch (final IOException e) {
            throw new WalkerException("Error when reading file \"" + path + "\"", e);
        }
    }
}
