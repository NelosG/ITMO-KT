package info.kgeorgiy.ja.pushkarev.hello;

import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.Objects;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.TimeUnit;

public class Helper {

    public static final Charset CHARSET = StandardCharsets.UTF_8;

    public static String getArg(final int i, final String[] args) {
        return Objects.requireNonNull(args[i]);
    }

    public static int getIntArg(final int i, final String[] args) {
        return Integer.parseInt(getArg(i, args));
    }

    public static void waitShutdown(final ExecutorService service, final long timeInMilliseconds) {
        if (service == null) {
            return;
        }
        service.shutdown();
        while (true) {
            try {
                if (service.awaitTermination(timeInMilliseconds, TimeUnit.MILLISECONDS)) {
                    break;
                }
            } catch (final InterruptedException e) {
                System.err.println("Executing thread interrupted during server shutdown: " + e.getMessage());
            }
            service.shutdownNow();
        }
    }

}
