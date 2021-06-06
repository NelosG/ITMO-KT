package info.kgeorgiy.ja.pushkarev.hello;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.DatagramChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;


public class HelloUDPNonblockingClient extends AbstractHelloClient {
    public static void main(final String[] args) {
        run(args, HelloUDPNonblockingClient::new);
    }

    @Override
    public void run(final String host, final int port, final String prefix, final int threads, final int requests) {
        final SocketAddress address = new InetSocketAddress(host, port);
        final List<DatagramChannel> channels = createChannels(threads);

        try (final Selector selector = Selector.open()) {
            for (int i = 0; i < channels.size(); ++i) {
                channels.get(i).register(selector, SelectionKey.OP_WRITE, new Attachment(i, requests,
                        channels.get(i).socket().getReceiveBufferSize()));
                channels.get(i).connect(address);
            }
            while (!Thread.interrupted() && !selector.keys().isEmpty()) {
                if (selector.select(SO_TIMEOUT_MILLISECONDS) == 0) {
                    writeInterest(selector);
                    continue;
                }
                final Iterator<SelectionKey> iterator = selector.selectedKeys().iterator();
                while (iterator.hasNext()) {
                    final SelectionKey key = iterator.next();
                    write(prefix, key, address);

                    read(key);

                    iterator.remove();
                }
            }
        } catch (final IOException e) {
            System.err.println("Server communication error: " + e.getMessage());
        }
        closeChannels(channels);
    }

    private void writeInterest(final Selector selector) {
        for (final SelectionKey selectionKey : selector.keys()) {
            selectionKey.interestOps(SelectionKey.OP_WRITE);
        }
    }

    private List<DatagramChannel> createChannels(final int threads) {
        final List<DatagramChannel> channels = new ArrayList<>();
        try {
            for (int i = 0; i < threads; i++) {
                final DatagramChannel channel = DatagramChannel.open();
                channel.configureBlocking(false);
                channels.add(channel);
            }
            return channels;
        } catch (final IOException e) {
            System.err.println("Can't create channels: " + e.getMessage());
            closeChannels(channels);
            return List.of();
        }
    }

    private void closeChannels(final List<DatagramChannel> channels) {
        for (final DatagramChannel channel : channels) {
            try {
                channel.close();
            } catch (final IOException e) {
                System.err.println("Error occurred when trying to close channels:" + e.getMessage());
            }
        }
    }

    private void write(final String prefix, final SelectionKey key, final SocketAddress address) throws IOException {
        if (key.isWritable()) {
            final Attachment attachment = (Attachment) key.attachment();
            ((DatagramChannel) key.channel()).send(
                    // :NOTE: Новый ByteBuffer
                    ByteBuffer.wrap(
                            getRequest(prefix, attachment.threadId, attachment.requestId).getBytes(Helper.CHARSET))
                    , address);
            key.interestOps(SelectionKey.OP_READ);
        }
    }

    private void read(final SelectionKey key) throws IOException {
        if (key.isReadable()) {
            final Attachment attachment = (Attachment) key.attachment();
            final DatagramChannel channel = (DatagramChannel) key.channel();
            channel.receive(attachment.buffer.clear());
            final String response = Helper.CHARSET.decode(attachment.buffer.flip()).toString();
            if (isResponseValid(response, attachment.threadId, attachment.requestId)) {
                attachment.requestId++;
            } else {
                System.err.println("Received invalid response: " + response);
            }
            if (attachment.requestId >= attachment.maxRequests) {
                channel.close();
                key.cancel();
            } else {
                key.interestOps(SelectionKey.OP_WRITE);
            }
        }
    }

    private static class Attachment {
        private final int threadId;
        private final int maxRequests;
        private final ByteBuffer buffer;
        private int requestId = 0;

        public Attachment(final int threadId, final int maxRequests, final int buffSize) {
            this.threadId = threadId;
            this.maxRequests = maxRequests;
            buffer = ByteBuffer.allocate(buffSize);
        }
    }
}
