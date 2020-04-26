import java.util.Scanner;
import java.util.Stack;

public class TurtleAndCoins {
    static int[][] dp;
    static char[][] ar;

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        int n = sc.nextInt();
        int m = sc.nextInt();
        dp = new int[n][m];
        ar = new char[n][m];
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                dp[i][j] = sc.nextInt();
            }
        }
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                dp[i][j] += max(i - 1, j, i, j - 1);
            }
        }
        StringBuilder sb = new StringBuilder();
        sb.append(dp[n - 1][m - 1] + "\n");
        dp = new int[0][0];
        System.gc();
        Stack<Character> ans = new Stack<>();
        int x = n - 1;
        int y = m - 1;
        while (x != 0 || y != 0) {
            ans.add(ar[x][y]);
            if (ar[x][y] == 'D') {
                x -= 1;
            } else if (ar[x][y] == 'R') {
                y -= 1;
            }
        }

        int size = ans.size();
        for (int i = 0; i < size; i++) {
            sb.append(ans.pop());
        }
        System.out.println(sb.toString());
    }

    static int max(int ia, int ja, int ib, int jb) {
        if (ia >= 0 && jb >= 0) {
            if (dp[ia][ja] > dp[ib][jb]) {
                ar[ia + 1][ja] = 'D';
                return dp[ia][ja];
            }
            ar[ib][jb + 1] = 'R';

            return dp[ib][jb];
        } else {
            if (ia >= 0) {
                ar[ia + 1][ja] = 'D';
                return dp[ia][ja];
            } else {
                if (jb >= 0) {
                    ar[ib][jb + 1] = 'R';
                    return dp[ib][jb];
                }
            }
        }
        return 0;
    }
}