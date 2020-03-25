package expression.exceptions;


public class ArithmeticMyException extends ArithmeticException {
    public ArithmeticMyException(String message) {
        super(message + "\n");
    }
}
