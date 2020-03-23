package expression.generic.RIBiD;

import java.math.BigInteger;

public class Bi extends R<BigInteger> {
    @Override
    public BigInteger add(BigInteger a, BigInteger b) {
        return a.add(b);
    }

    @Override
    public BigInteger sub(BigInteger a, BigInteger b) {
        return a.subtract(b);
    }

    @Override
    public BigInteger mul(BigInteger a, BigInteger b) {
        return a.multiply(b);
    }

    @Override
    public BigInteger div(BigInteger a, BigInteger b) {
        if (b == BigInteger.ZERO) return null;
        return a.divide(b);
    }

    @Override
    public BigInteger con(String str) {
        return new BigInteger(str);
    }
}
