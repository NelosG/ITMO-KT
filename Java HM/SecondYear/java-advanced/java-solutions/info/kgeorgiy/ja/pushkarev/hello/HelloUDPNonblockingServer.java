package info.kgeorgiy.ja.pushkarev.hello;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.net.StandardSocketOptions;
import java.nio.ByteBuffer;
import java.nio.channels.ClosedSelectorException;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.ArrayDeque;
import java.util.Iterator;
import java.util.Queue;
import java.util.concurrent.Executors;
import java.util.stream.Collectors;
import java.util.stream.Stream;


public class HelloUDPNonblockingServer extends AbstractHelloServer {

    private DatagramChannel channel;
    private Selector selector;
    private Thread controlThread;

    public static void main(final String[] args) {
        run(args, HelloUDPNonblockingServer::new);
    }

    private void run() {
        while (!Thread.currentThread().isInterrupted() && !channel.socket().isClosed()) {
            try {
                selector.select();
                final Iterator<SelectionKey> iter = selector.selectedKeys().iterator();
                while (iter.hasNext()) {
                    final SelectionKey key = iter.next();
                    try {
                        write(key);
                        read(key);
                    } finally {
                        iter.remove();
                    }
                }
            } catch (final IOException e) {
                System.err.println("Client communication error: " + e.getMessage());
                close();
            } catch (final ClosedSelectorException e) {
                System.out.println("Selector was closed");
            }
        }
    }

    private void read(final SelectionKey key) {
        if (key.isReadable()) {

            try {
                final Attachment attachment = (Attachment) key.attachment();
                final Attachment.BufferContainer requestBuffer = attachment.requestBuffers.remove();
                if (attachment.requestBuffers.isEmpty()) {
                    key.interestOpsAnd(~SelectionKey.OP_READ);
                }
                requestBuffer.address = channel.receive(requestBuffer.receive.clear());
                serverPool.submit(() -> {
                    final String request = Helper.CHARSET.decode(requestBuffer.receive.flip()).toString();
                    requestBuffer.send = ByteBuffer.wrap(response(request).getBytes(Helper.CHARSET));
                    synchronized (attachment.responseBuffers) {
                        attachment.responseBuffers.add(requestBuffer);
                        if ((key.interestOps() & SelectionKey.OP_WRITE) == 0) {
                            key.interestOpsOr(SelectionKey.OP_WRITE);
                            selector.wakeup();
                        }
                    }
                });
            } catch (final IOException e) {
                System.err.println("Data receiving (from client) error: " + e.getMessage());
                close();
            }
        }
    }

    private void write(final SelectionKey key) {
        if (key.isWritable()) {

            try {
                final Attachment attachment = (Attachment) key.attachment();
                final Attachment.BufferContainer bufferToSend;
                synchronized (attachment.responseBuffers) {
                    bufferToSend = attachment.responseBuffers.remove();
                    if (attachment.responseBuffers.isEmpty()) {
                        key.interestOpsAnd(~SelectionKey.OP_WRITE);
                    }
                }
                channel.send(bufferToSend.send, bufferToSend.address);
                attachment.requestBuffers.add(bufferToSend);
                key.interestOpsOr(SelectionKey.OP_READ);
            } catch (final IOException e) {
                System.err.println("Data sending (to client) error: " + e.getMessage());
                close();
            }
        }
    }

    @Override
    protected void init(final int port, final int threads) {
        try {
            selector = Selector.open();
            channel = DatagramChannel.open();
            channel.setOption(StandardSocketOptions.SO_REUSEADDR, true);
            channel.configureBlocking(false);
            channel.register(selector, SelectionKey.OP_READ, new Attachment(threads,
                    channel.socket().getReceiveBufferSize()));
            channel.bind(new InetSocketAddress(port));
            serverPool = Executors.newFixedThreadPool(threads);
            controlThread = new Thread(this::run);
            controlThread.start();
        } catch (final IOException e) {
            System.err.println("Error occurred during server setup: " + e.getMessage());
        }
    }

    @Override
    protected void doClose() {
        try {
            if (controlThread != null) {
                controlThread.interrupt();
                try {
                    controlThread.join();
                } catch (InterruptedException e) {
                    e.printStackTrace();
                }
            }
            if (channel != null) {
                channel.close();
            }
            if (selector != null) {
                selector.close();
            }
            Helper.waitShutdown(serverPool, Long.MAX_VALUE);
        } catch (final IOException e) {
            System.err.println("Resources closing error: " + e.getMessage());
        }
    }

    private class Attachment {
        private final Queue<BufferContainer> requestBuffers;
        private final Queue<BufferContainer> responseBuffers;

        private Attachment(final int threads, final int buffSize) {
            this.requestBuffers = Stream.generate(() -> new BufferContainer(buffSize))
                    .limit(threads)
                    .collect(Collectors.toCollection(ArrayDeque::new));
            this.responseBuffers = new ArrayDeque<>();

        }

        private class BufferContainer {
            ByteBuffer receive;
            ByteBuffer send;
            SocketAddress address;

            public BufferContainer(final int buffSize) {
                receive = ByteBuffer.allocate(buffSize);
            }
        }
    }
}
