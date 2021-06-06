import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Scanner;

public class E {
    static HashMap<Character, HashSet<String>> P = new HashMap<>();
    static Character start;
    static HashMap<String, Boolean>[][][][] h;
    static Boolean[][][] a;

    public static void main(String[] args) throws IOException {
        Scanner sc = new Scanner(System.in);
        String s = sc.nextLine(), tmp;
        Scanner scc = new Scanner(s);
        char tempChar;
        int n = scc.nextInt();
        start = scc.next().charAt(0);
        h = new HashMap[26][n + 2][2 * n + 3][6];
        a = new Boolean[52][n + 4][n + 4];
        for (int i = 0; i < 26; i++) {
            for (int l = 0; l <= n + 1; l++) {
                for (int j = 0; j <= 2 * n + 2; j++) {
                    for (int k = 0; k <= 5; k++) {
                        h[i][l][j][k] = new HashMap<>();
                    }
                }
            }
        }
        for (int i = 0; i < 52; i++) {
            for (int j = 0; j <= n + 3; j++) {
                for (int k = 0; k <= n + 3; k++) {
                    a[i][j][k] = false;
                }
            }
        }
        for (int i = 0; i < n; i++) {
            s = sc.nextLine();
            tmp = "";
            scc = new Scanner(s);
            tempChar = scc.next().charAt(0);
            scc.next();
            if (s.length() > 5) {
                tmp = scc.next();
            }
            if (!P.containsKey(tempChar))
                P.put(tempChar, new HashSet<>());
            P.get(tempChar).add(tmp);
        }
        System.out.println(check(sc.next()));
    }

    static boolean check(String S) { // S — строка длины n, Г — КС-грамматика
        int n = S.length();
        for (int i = 1; i <= n; i++) {
            for (Map.Entry<Character, HashSet<String>> A : P.entrySet()) { // перебор состояний
                a[A.getKey() - 'A'][i][i + 1] = A.getValue().contains(S.charAt(i - 1)); // если в грамматике Г присутствует правило A -> w[i]
                a[A.getKey() - 'A'][i][i] = A.getValue().contains(""); // если в грамматике Г присутствует правило A -> eps
                for (String alpha : A.getValue()) {
                    h[A.getKey() - 'A'][i][i][0].put(alpha, true);
                }
            }
        }
        for (int m = 1; m <= n; m++) {
            for (int i = 1; i <= n; i++) {
                int j = i + m;
                for (int k = 1; k <= 5; k++) {
                    for (Map.Entry<Character, HashSet<String>> A : P.entrySet()) { // перебор состояний
                        for (String alpha : A.getValue()) {
                            boolean temp = false;
                            if (k < alpha.length()) {
                                for (int r = i; r <= j + 1; r++) {
                                    int tmp;
                                    if (alpha.charAt(k - 1) >= 'A' && alpha.charAt(k - 1) <= 'Z')
                                        tmp = alpha.charAt(k - 1) - 'A';
                                    else tmp = alpha.charAt(k - 1) - 'a' + 26;
                                    Object tem = h[A.getKey() - 'A'][i][r][k - 1].get(alpha);
                                    temp |= ((tem == null ? false : (Boolean) tem) & a[tmp][r][j + 1]);
                                }
                            }
                            h[A.getKey() - 'A'][i][j + 1][k].put(alpha, temp);
                        }
                    }
                }
            }
        }
        for (int i = 1; i <= n; i++) {
            for (int j = 1; j <= n; j++) {
                for (Map.Entry<Character, HashSet<String>> A : P.entrySet()) {
                    boolean temp = false;
                    for (String alpha : A.getValue()) {
                        Object tem = h[A.getKey() - 'A'][i][j][alpha.length()].get(alpha);
                        temp |= (tem == null ? false : (Boolean) tem); // где |alpha| — размер правой части правила
                    }
                    a[A.getKey() - 'A'][i][j] = temp;
                }
            }
        }
        return a[start - 'A'][1][n];
    }
}
