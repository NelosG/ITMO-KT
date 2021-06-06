package info.kgeorgiy.ja.pushkarev.hello;

import info.kgeorgiy.java.advanced.hello.HelloClient;

import java.util.function.Supplier;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public abstract class AbstractHelloClient implements HelloClient {

    protected static final int SO_TIMEOUT_MILLISECONDS = 100;

    private final Pattern RESPONSE_PATTERN = Pattern.compile("[\\D]*([\\d]+)[\\D]*([\\d]+)[\\D]*");

    protected static void run(final String[] args, final Supplier<HelloClient> clientSupplier) {
        try {
            if (args.length != 5) {
                System.err.println("Usage of HelloClient: <host> <port> <prefix> <threads> <requests>");
                return;
            }
            clientSupplier.get().run(Helper.getArg(0, args),
                    Helper.getIntArg(1, args),
                    Helper.getArg(2, args),
                    Helper.getIntArg(3, args),
                    Helper.getIntArg(4, args));
        } catch (final NumberFormatException | NullPointerException e) {
            System.err.println("Invalid argument(s): " + e.getMessage());
        }
    }

    protected boolean isResponseValid(final String response, final int threadId, final int requestId) {
        Matcher matcher = RESPONSE_PATTERN.matcher(response);
        if (!matcher.find()) {
            return false;
        }
        return Integer.toString(threadId).equals(matcher.group(1))
                && Integer.toString(requestId).equals(matcher.group(2));
    }

    protected String getRequest(final String prefix, final int threadId, final int requestId) {
        return String.format("%s%s_%s", prefix, threadId, requestId);
    }

}
