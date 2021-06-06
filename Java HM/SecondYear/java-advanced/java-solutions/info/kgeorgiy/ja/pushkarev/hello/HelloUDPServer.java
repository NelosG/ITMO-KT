package info.kgeorgiy.ja.pushkarev.hello;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;
import java.net.SocketException;
import java.net.SocketTimeoutException;
import java.util.concurrent.Executors;


public class HelloUDPServer extends AbstractHelloServer {

    private DatagramSocket socket;

    public static void main(final String[] args) {
        run(args, HelloUDPServer::new);
    }

    @Override
    protected void init(final int port, final int threads) {
        serverPool = Executors.newFixedThreadPool(threads);
        try {
            socket = new DatagramSocket(port);
            for (int i = 0; i < threads; i++) {
                serverPool.submit(this::process);
            }
        } catch (final SocketException e) {
            System.err.println("Error when creating socket: " + e.getMessage());
        }
    }

    private void process() {
        try {
            final byte[] receive = new byte[socket.getReceiveBufferSize()];
            final DatagramPacket packet = new DatagramPacket(receive, receive.length);
            while (!socket.isClosed() && !Thread.currentThread().isInterrupted()) {
                try {
                    Utils.send(response(Utils.setDataAndReceive(receive, packet, socket)),
                            packet,
                            socket);
                } catch (final SocketTimeoutException e) {
                    System.err.println("Server did not receive any requests in given time");
                } catch (final SocketException e) {
                    if (!closed) {
                        System.err.println("Client communication error: " + e.getMessage());
                    }
                    return;
                } catch (final IOException e) {
                    System.err.println("Client communication error: " + e.getMessage());
                }
            }
        } catch (final SocketException e) {
            System.err.println("UDP packet creation error: " + e.getMessage());
        }
    }

    @Override
    protected void doClose() {
        Helper.waitShutdown(serverPool, Long.MAX_VALUE);
        socket.close();
    }
}
