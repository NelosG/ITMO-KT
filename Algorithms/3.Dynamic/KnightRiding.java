import java.util.Scanner;

public class KnightRiding {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        int n = sc.nextInt();
        long[][] dp = new long[n + 1][10];
        for (int i = 0; i < 10; i++) {
            dp[0][i] = 1;
            dp[1][i] = 1;
        }
        dp[1][0] = 0;
        dp[1][8] = 0;
        for (int i = 2; i <= n; i++) {
            dp[i][0] = (dp[i - 1][4] + dp[i - 1][6]) % 1_000_000_000;
            dp[i][1] = (dp[i - 1][6] + dp[i - 1][8]) % 1_000_000_000;
            dp[i][2] = (dp[i - 1][7] + dp[i - 1][9]) % 1_000_000_000;
            dp[i][3] = (dp[i - 1][4] + dp[i - 1][8]) % 1_000_000_000;
            dp[i][4] = (dp[i - 1][0] + dp[i - 1][3] + dp[i - 1][9]) % 1_000_000_000;
            dp[i][6] = (dp[i - 1][0] + dp[i - 1][1] + dp[i - 1][7]) % 1_000_000_000;
            dp[i][7] = (dp[i - 1][2] + dp[i - 1][6]) % 1_000_000_000;
            dp[i][8] = (dp[i - 1][1] + dp[i - 1][3]) % 1_000_000_000;
            dp[i][9] = (dp[i - 1][4] + dp[i - 1][2]) % 1_000_000_000;

        }
        long res = 0l;
        for (int i = 0; i < 10; i++) {
            res += dp[n][i];
        }
        System.out.println(res % 1_000_000_000);
    }
}