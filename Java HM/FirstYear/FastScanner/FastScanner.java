import java.io.*;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.util.InputMismatchException;
import java.util.NoSuchElementException;

public class FastScanner implements AutoCloseable {


    private int pos;
    private int len;
    private char[] buffer;
    private boolean EOF = false;
    private boolean closed = false;
    private final Reader is;

    private final int NORMAL_BUFFER_SIZE = 300;
    private int BUFFER_SIZE = NORMAL_BUFFER_SIZE;
    private int foundedNextInt;
    private int rollback_count = 0;


    public FastScanner(InputStream in) {
        this(in, StandardCharsets.UTF_8);
    }

    public FastScanner(InputStream in, Charset charset) {
        buffer = new char[BUFFER_SIZE];
        is = new InputStreamReader(in, charset);
    }

    public FastScanner(String in) {
        this(in, StandardCharsets.UTF_8);
    }

    public FastScanner(String in, Charset charset) {
        this(new ByteArrayInputStream((in + " ").getBytes(charset)), charset);
    }

    public FastScanner(File in) throws IOException {
        this(in, StandardCharsets.UTF_8);
    }

    public FastScanner(File in, Charset charset) throws IOException {
        this(new FileInputStream(in), charset);
    }


    public char nextChar() throws IOException {
        checkState();
        if (pos >= len) {
            readBuffer();
        }
        if (EOF) {
            throw new NoSuchElementException("End of file");
        }
        return buffer[pos++];
    }

    private void checkState() {
        if (closed) {
            throw new IllegalStateException("FastScanner is closed");
        }
    }


    public boolean hasNextChar() throws IOException {
        checkState();
        return pos < len || is.ready();
    }


    public boolean hasNextLine() throws IOException {
        return hasNextChar();
    }

    public String nextLine() throws IOException {
        StringBuilder sb = new StringBuilder();
        char c;
        while (hasNextChar()) {
            c = nextChar();
            if (c == '\n') {
                break;
            }
            if (c != '\r') {
                sb.append(c);
            }
        }
        return sb.toString();
    }


    public boolean hasNextInt() throws IOException {
        return hasNextIntImpl(true);
    }

    private boolean hasNextIntImpl(boolean isOnlyCheck) throws IOException {
        skipBlank(isOnlyCheck);
        char c;
        int maxSymbols = 10;
        if (!hasNextChar()) {
            if (!isOnlyCheck) {
                throw new NoSuchElementException();
            }
            return false;
        } else {
            incRollback(isOnlyCheck);
            c = nextChar();
            StringBuilder sb = new StringBuilder();
            if (c == '-' || c == '+') {
                sb.append(c);
                incRollback(isOnlyCheck);
                c = nextChar();
                ++maxSymbols;
            }
            if (!Character.isDigit(c)) {
                rollback();
                return false;
            }


            while (Character.isDigit(c)) {
                sb.append(c);
                if (sb.length() > maxSymbols) {
                    rollback();
                    return false;
                }
                incRollback(isOnlyCheck);
                c = nextChar();
            }

            rollback();

            if (!Character.isWhitespace(c)) {
                return false;
            } else {
                if (!isOnlyCheck) {
                    try {
                        foundedNextInt = Integer.parseInt(sb.toString());
                    } catch (NumberFormatException e) {
                        return false;
                    }
                }
            }
        }
        return true;
    }

    public int nextInt() throws IOException {
        if (hasNextIntImpl(false)) {
            return foundedNextInt;
        }
        throw new InputMismatchException();
    }

    private void incRollback(boolean wantToRollback) throws IOException {
        if (wantToRollback) {
            ++rollback_count;
            if (pos >= BUFFER_SIZE - 2) {
                readBuffer();
            }
        }
    }

    public boolean hasNext() throws IOException {
        skipBlank(true);
        boolean res = hasNextChar();
        rollback();
        return res;
    }

    private void rollback() {
        pos -= rollback_count;
        rollback_count = 0;
    }

    public String next() throws IOException {
        skipBlank(false);
        StringBuilder sb = new StringBuilder();
        char c;
        while (hasNextChar()) {
            incRollback(true);
            c = nextChar();
            if (c == '\n' && sb.length() != 0) {
                rollback();
                break;
            }
            --rollback_count;
            if (c != ' ') {
                sb.append(c);
            } else {
                break;
            }
        }
        if (sb.length() != 0) {
            return sb.toString();
        } else {
            throw new NoSuchElementException();
        }
    }

    @Override
    public void close() throws IOException {
        closed = true;
        is.close();
    }


    //TODO::Bad complex function, need to fix
    private void readBuffer() throws IOException {
        if (rollback_count > 0) {
            if (pos >= BUFFER_SIZE - 2) {
                BUFFER_SIZE *= 2;
                char[] tempBuffer = new char[BUFFER_SIZE];
                len = len - pos + rollback_count;
                System.arraycopy(buffer, pos - rollback_count, tempBuffer, 0, len);
                pos = rollback_count;
                buffer = tempBuffer;
                int prevLen = len;
                len = 0;
                while (len == 0) {
                    len = is.read(buffer, prevLen, BUFFER_SIZE - prevLen);
                }
                len += prevLen;
                return;
            } else {
                System.arraycopy(buffer, BUFFER_SIZE - rollback_count, buffer, 0, rollback_count);
            }
        }
        len = 0;
        while (len == 0) {
            len = is.read(buffer, rollback_count, BUFFER_SIZE - rollback_count);
        }
        if (len == -1) {
            EOF = true;
        }
        len += rollback_count;
        pos = rollback_count;

        if (len < BUFFER_SIZE / 2) {
            int prevBuffSize = BUFFER_SIZE;
            BUFFER_SIZE = Math.max(len, NORMAL_BUFFER_SIZE);
            if (prevBuffSize != BUFFER_SIZE) {
                char[] tempBuffer = new char[BUFFER_SIZE];
                System.arraycopy(buffer, 0, tempBuffer, 0, len);
                buffer = tempBuffer;
            }
        }
    }


    private void skipBlank(boolean wantToRollback) throws IOException {
        while (hasNextChar()) {
            ++rollback_count;
            if (!Character.isWhitespace(nextChar())) {
                --pos;
                --rollback_count;
                break;
            }
            if (!wantToRollback) {
                --rollback_count;
            }
        }
    }
}
