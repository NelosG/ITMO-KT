import java.util.Scanner;

public class Stars {
    static int n;
    static int[][][] ar;

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        n = sc.nextInt();
        ar = new int[n][n][n];
        while (true) {
            int s = sc.nextInt();
            if (s == 1) {
                inc(sc.nextInt(), sc.nextInt(), sc.nextInt(), sc.nextInt());
            } else {
                if (s == 2) {
                    int x1 = sc.nextInt() - 1;
                    int y1 = sc.nextInt() - 1;
                    int z1 = sc.nextInt() - 1;
                    int x2 = sc.nextInt();
                    int y2 = sc.nextInt();
                    int z2 = sc.nextInt();
                    System.out.println(sum(x2, y2, z2) - sum(x2, y1, z2) - sum(x1, y2, z2)
                            - sum(x2, y2, z1) + sum(x2, y1, z1) + sum(x1, y2, z1)
                            + sum(x1, y1, z2) - sum(x1, y1, z1));
                } else {
                    break;
                }
            }
        }
    }

    static int sum(int x, int y, int z) {
        int result = 0;
        for (int i = x; i >= 0; i = (i & (i + 1)) - 1) {
            for (int j = y; j >= 0; j = (j & (j + 1)) - 1) {
                for (int k = z; k >= 0; k = (k & (k + 1)) - 1) {
                    result += ar[i][j][k];
                }
            }
        }
        return result;
    }

    static void inc(int x, int y, int z, int delta) {
        for (int i = x; i < n; i = (i | (i + 1))) {
            for (int j = y; j < n; j = (j | (j + 1))) {
                for (int k = z; k < n; k = (k | (k + 1))) {
                    ar[i][j][k] += delta;
                }
            }
        }
    }
}