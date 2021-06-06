import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class D {
    private static BigInteger Pow(BigInteger a, long b) {
        BigInteger result = BigInteger.ONE;
        for (long i = 0; i < b; i = ++i) {
            result = result.multiply(a);
        }
        return result;
    }

    private static BigInteger Factorial(long n) {
        BigInteger result = BigInteger.ONE;
        for (long i = 2; i <= n; ++i) {
            result = result.multiply(BigInteger.valueOf(i));
        }
        return result;
    }

    private static List<BigInteger> MultiplyGenFunctions(List<BigInteger> first, List<BigInteger> second) {
        List<BigInteger> result = new ArrayList<>();
        for(int i = 0; i < first.size() + second.size() - 1; i++) {
            result.add(BigInteger.ZERO);
        }
        for (int i = 0; i < first.size(); i++) {
            for (int j = 0; j < second.size(); j++) {
                //result[i + j] += first[i] * second[j];
                result.set(i + j, result.get(i + j).add(first.get(i).multiply(second.get(j))));
            }
        }

        return result;
    }

    public static void main(String[] args) {
        long r, k;
        Scanner sc = new Scanner(System.in);
        r = sc.nextLong();
        k = sc.nextLong();
        List<BigInteger> coefficients = new ArrayList<>();
        for(int i = 0; i < k + 1; i++) {
            coefficients.add(BigInteger.valueOf(sc.nextLong()));
        }

        List<BigInteger> result = new ArrayList<>();
        for(int i = 0; i < k + 1; i++) {
            result.add(BigInteger.ZERO);
        }
        BigInteger denominator = Factorial(k).multiply(Pow(BigInteger.valueOf(r), k));

        for (int i = 0; i <= k; i++) {
            List<BigInteger> cur = new ArrayList<>();
            cur.add(coefficients.get(i).prod(Pow(BigInteger.valueOf(r), k - i)));

            for (int j = 1; j <= k; j++) {
                List<BigInteger> temp = new ArrayList<>();
                temp.add(BigInteger.valueOf(j-i));
                temp.add(BigInteger.ONE);

                cur = MultiplyGenFunctions(cur, temp);
            }

            for (int j = 0; j <= k; j++) {
                result.set(j, result.get(j).add(cur.get(j)));
            }
        }
        StringBuilder sb = new StringBuilder();
        result.forEach(numerator -> {
            BigInteger GCD = denominator.gcd(numerator.abs());
            sb.append(numerator.divide(GCD)).append("/").append(denominator.divide(GCD)).append(" ");
        });
        System.out.println(sb);
    }
}
