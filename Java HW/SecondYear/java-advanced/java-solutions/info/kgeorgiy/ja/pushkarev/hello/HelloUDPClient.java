package info.kgeorgiy.ja.pushkarev.hello;

import java.io.IOException;
import java.net.*;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;


public class HelloUDPClient extends AbstractHelloClient {

    public static void main(final String[] args) {
        run(args, HelloUDPClient::new);
    }

    @Override
    public void run(final String host, final int port, final String prefix, final int threads, final int requests) {
        final SocketAddress address = new InetSocketAddress(host, port);
        final ExecutorService requesters = Executors.newFixedThreadPool(threads);
        for (int id = 0; id < threads; id++) {
            final int threadId = id;
            requesters.submit(() -> process(prefix, threadId, requests, address));
        }
        Helper.waitShutdown(requesters, Long.MAX_VALUE);
    }

    private void process(final String prefix, final int threadId, final int requests, final SocketAddress address) {
        try (final DatagramSocket socket = new DatagramSocket()) {
            socket.setSoTimeout(SO_TIMEOUT_MILLISECONDS);
            final DatagramPacket packet = new DatagramPacket(new byte[0], 0, address);
            final byte[] receive = new byte[socket.getReceiveBufferSize()];
            for (int id = 0; id < requests; id++) {
                final String request = getRequest(prefix, threadId, id);
                while (!Thread.currentThread().isInterrupted() && !socket.isClosed()) {
                    try {
                        Utils.send(request, packet, socket);
                        final String response = Utils.setDataAndReceive(receive, packet, socket);
                        if (isResponseValid(response, threadId, id)) {
                            break;
                        } else {
                            System.err.println("Received invalid response \"" + response + "\" to request \"" + request + "\"");
                        }
                    } catch (final SocketTimeoutException ignored) {
                    } catch (final IOException e) {
                        System.err.println("Server communication error: " + e.getMessage());
                    }
                }
            }
        } catch (final SocketException e) {
            System.err.println("Socket creation error: " + e.getMessage());
        }
    }
}
