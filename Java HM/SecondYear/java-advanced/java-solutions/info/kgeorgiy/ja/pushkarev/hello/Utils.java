package info.kgeorgiy.ja.pushkarev.hello;

import java.io.IOException;
import java.net.DatagramPacket;
import java.net.DatagramSocket;

public class Utils {

    public static void send(final String request, final DatagramPacket packet,
                            final DatagramSocket socket) throws IOException {
        packet.setData(request.getBytes(Helper.CHARSET));
        socket.send(packet);
    }

    public static String setDataAndReceive(final byte[] receive,
                                           final DatagramPacket packet,
                                           final DatagramSocket socket) throws IOException {
        packet.setData(receive);
        socket.receive(packet);
        return new String(packet.getData(), packet.getOffset(), packet.getLength(), Helper.CHARSET);
    }
}
