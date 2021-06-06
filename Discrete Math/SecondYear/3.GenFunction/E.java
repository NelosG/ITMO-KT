import java.util.ArrayList;
import java.util.List;
import java.util.Scanner;

public class E
{

    private static List<Long> MultiplyVec(List<Long> first, List<Long> second, long m) {
        List<Long> result = new ArrayList<>();
        for(int i = 0; i < Math.min(m, first.size() + second.size() - 1); i++) {
            result.add(0L);
        }
        for (int i = 0; i < first.size(); i++) {
            for (int j = 0; j < second.size() && i + j < m; j++) {
                result.set(i + j, result.get(i + j) + first.get(i) * second.get(j));
            }
        }

        return result;
    }

    private static long Pow(long a, long b)
    {
        var result = 1L;
        for (long i = 0; i < b; i++)
        {
            result *= a;
        }
        return result;
    }

    private static String ListToString(List<Long> result)
    {
        StringBuilder stringBuilder = new StringBuilder();
        stringBuilder.append(result.size() - 1).append('\n');
        for(Long num : result)
        {
            stringBuilder.append(num).append(' ');
        }
        return stringBuilder.toString();
    }

    public static void main(String[] args)
    {
        Scanner sc = new Scanner(System.in);
        long r = sc.nextLong();
        long d = sc.nextLong();

        List<Long> genFunction = new ArrayList<>();
        for(int i = 0; i < d + 1; i++) {
            genFunction.add(sc.nextLong());
        }
        List<Long> denominator = new ArrayList<>();
        denominator.add(1L);
        denominator.add(-r);
        List<Long> temp = new ArrayList<>();
        temp.add(1L);
        temp.add(-r);
        for (int i = 0; i < d; i++)
        {
            denominator = MultiplyVec(denominator, temp, d + 2);
        }

        List<Long> coefficients = new ArrayList<>();
        for(int i = 0; i < d + 1; i++) {
            coefficients.add(0L);
        }
        for (long n = 0; n <= d; n++)
        {
            long x = 1L;
            for (int i = 0; i < genFunction.size(); i++, x *= n)
            {
                coefficients.set((int)n, coefficients.get((int)n) + x * genFunction.get(i) * Pow(r, n));
            }
        }

        List<Long> numerator = MultiplyVec(coefficients, denominator, (d + 1));
        System.out.println(ListToString(numerator));
        System.out.println(ListToString(denominator));
    }
}
