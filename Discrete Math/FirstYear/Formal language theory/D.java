import java.io.FileInputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.*;

public class D {
    static long MOD = 1000000007;
    static HashMap<Character, HashSet<String>> R = new HashMap<>();
    static Character start;

    public static void main(String[] args) throws IOException {
        Scanner sc = new Scanner(new FileInputStream("nfc.in"));
        PrintWriter fr = new PrintWriter("nfc.out");
        char tempChar;
        int n = sc.nextInt();
        start = sc.next().charAt(0);
        for (int i = 0; i < n; i++) {
            tempChar = sc.next().charAt(0);
            sc.next();
            if (!R.containsKey(tempChar)) R.put(tempChar, new HashSet<>());
            R.get(tempChar).add(sc.next());
        }
        long res = check(sc.next());
        fr.print(res);
        fr.close();
    }

    static long check(String S) {
        int n = S.length();
        long[][][] d = new long[26][n][n];
        for (int i = 0; i < n; i++)
            for (Map.Entry<Character, HashSet<String>> A : R.entrySet())
                for (String str : A.getValue())
                    if (str.length() == 1 && str.charAt(0) == S.charAt(i)) {
                        d[A.getKey() - 'A'][i][i] = 1;
                        break;
                    }

        for (int i = 0; i < n; i++)
            for (int j = 0; j < n - i; j++)
                if (i + j < n)
                    for (int k = j; k < i + j; k++)
                        for (Map.Entry<Character, HashSet<String>> A : R.entrySet())
                            for (String str : A.getValue())
                                if (str.length() == 2)
                                    d[A.getKey() - 'A'][j][i + j] = (d[A.getKey() - 'A'][j][i + j] + d[str.charAt(0) - 'A'][j][k] * d[str.charAt(1) - 'A'][k + 1][i + j]) % MOD;
        return (d[start - 'A'][0][n - 1]);
    }
}