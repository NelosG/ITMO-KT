import java.util.Scanner;

public class RSQ {
    static long[] t;
    static int one;
    static int n;

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        n = sc.nextInt();
        one = 1;
        while (one < n) {
            one *= 2;
        }
        t = new long[2 * one - 1];
        for (int i = one - 1; i < one - 1 + n; i++) {
            t[i] = sc.nextLong();
        }
        for (int i = one - 2; i >= 0; i--) {
            t[i] = t[2 * i + 1] + t[2 * i + 2];
        }
        long sum = 0;
        while (sc.hasNext()) {
            String s = sc.next();
            int i = sc.nextInt() - 1;
            int x = sc.nextInt();
            if (s.equals("set")) {
                set(i, x);
            } else {
                System.out.println(sum(0, 0, one, i, x));
            }
        }
    }

    static void set(int i, long x) {
        int v = one - 1 + i;
        t[v] = x;
        if (v != 0) {
            update((v - 1) / 2);
        }
    }

    static void update(int v) {
        t[v] = t[2 * v + 1] + t[2 * v + 2];
        if (v != 0) {
            update((v - 1) / 2);
        }
    }

    static long sum(int v, int l, int r, int a, int b) {
        if (b <= l || a >= r) return 0;
        if (b >= r && a <= l) return t[v];
        return sum(2 * v + 1, l, (r + l) / 2, a, b) + sum(2 * v + 2, (r + l) / 2, r, a, b);
    }
}