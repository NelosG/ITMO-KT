package tokenizer;

public class TokenizerException extends Exception {
    public TokenizerException() {
        super();
    }

    public TokenizerException(String message) {
        super(message);
    }

    public TokenizerException(String message, Throwable cause) {
        super(message, cause);
    }

    @Override
    public boolean equals(Object obj) {
        if(obj.getClass() == TokenizerException.class) {
            return ((TokenizerException) obj).getMessage().equals(getMessage());
        }
        return false;
    }

    public TokenizerException(Throwable cause) {
        super(cause);
    }
}
