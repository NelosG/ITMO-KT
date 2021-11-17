package info.kgeorgiy.ja.pushkarev.hello;

import info.kgeorgiy.java.advanced.hello.HelloServer;

import java.util.concurrent.ExecutorService;
import java.util.function.Supplier;

public abstract class AbstractHelloServer implements HelloServer {

    protected boolean closed = true;
    protected ExecutorService serverPool;

    protected static void run(final String[] args, final Supplier<HelloServer> serverSupplier) {
        try {
            if (args.length != 2) {
                System.err.println("Usage of HelloServer: <port> <threads>");
                return;
            }
            try (HelloServer server = serverSupplier.get()) {
                server.start(Helper.getIntArg(0, args),
                        Helper.getIntArg(1, args));
            }
        } catch (final NumberFormatException | NullPointerException e) {
            System.err.println("Invalid argument(s): " + e.getMessage());
        }
    }

    @Override
    public void close() {
        if (!closed) {
            closed = true;
            doClose();
        }
    }

    @Override
    public void start(final int port, final int threads) {
        if (!closed) {
            throw new IllegalStateException("The server was already started.");
        }
        init(port, threads);
        closed = false;
    }

    protected String response(final String request) {
        return "Hello, " + request;
    }

    protected abstract void init(final int port, final int threads);

    protected abstract void doClose();
}
