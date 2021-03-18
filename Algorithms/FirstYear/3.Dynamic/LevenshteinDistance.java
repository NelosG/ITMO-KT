import java.util.Scanner;

public class LevenshteinDistance {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        String str1 = sc.nextLine();
        String str2 = sc.nextLine();
        System.out.println(li(str1, str2, str1.length(), str2.length()));
    }

    static int li(String s1, String s2, int n, int m) {
        int[][] dp = new int[n + 1][m + 1];
        dp[0][0] = 0;
        for (int j = 1; j <= m; j++) dp[0][j] = dp[0][j - 1] + 1;
        for (int i = 1; i <= n; i++) {
            dp[i][0] = dp[i - 1][0] + 1;
            for (int j = 1; j <= m; j++) {
                if (s1.charAt(i - 1) != s2.charAt(j - 1)) {
                    dp[i][j] = min(dp[i - 1][j] + 1, dp[i][j - 1] + 1, dp[i - 1][j - 1] + 1);
                } else {
                    dp[i][j] = dp[i - 1][j - 1];
                }
            }
        }
        return dp[n][m];
    }

    static int min(int a, int b, int c) {
        if (a > b) {
            if (b > c) return c;
            return b;
        }
        if (a > c) return c;
        return a;
    }

}