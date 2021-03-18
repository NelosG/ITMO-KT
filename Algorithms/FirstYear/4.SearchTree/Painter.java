import java.util.Scanner;

public class Painter {
    static int one = 1048576;
    static long[] sum = new long[one * 2 - 1];
    static long[] kol = new long[one * 2 - 1];
    static boolean[] left = new boolean[one * 2 - 1];
    static boolean[] right = new boolean[one * 2 - 1];
    static int[] per = new int[one * 2 - 1];

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        for (int i = 0; i < one * 2 - 1; i++) {
            per[i] = -1;

        }
        int n;
        n = sc.nextInt();
        for (int i = 0; i < n; i++) {
            String s = sc.next();
            int a = sc.nextInt();
            int b = sc.nextInt();

            if (s.equals("W")) {
                set(0, 0, one, a + 500000, a + b + 500000, 0);
            } else {
                set(0, 0, one, a + 500000, a + b + 500000, 1);
            }
            update(0, 0, one);
            System.out.println(kol[0] + " " + sum[0]);
        }
    }


    static void set(int v, int l, int r, int a, int b, int x) {
        push(v, l, r);
        if (a >= r || b <= l) return;
        if (a <= l && r <= b) {
            per[v] = x;
            return;
        }
        set(2 * v + 1, l, (l + r) / 2, a, b, x);
        set(2 * v + 2, (l + r) / 2, r, a, b, x);
        update(v, l, r);
        return;
    }

    static void push(int v, int l, int r) {
        if (per[v] == 0) {
            kol[v] = 0;
            sum[v] = 0;
            right[v] = false;
            left[v] = false;
            if (v < one - 1) {
                per[2 * v + 1] = 0;
                per[2 * v + 2] = 0;
            }
            per[v] = -1;
            return;
        }
        if (per[v] == 1) {
            kol[v] = 1;
            sum[v] = r - l;
            right[v] = true;
            left[v] = true;
            if (v < one - 1) {
                per[2 * v + 1] = 1;
                per[2 * v + 2] = 1;
            }
            per[v] = -1;
            return;
        }
        return;
    }

    static void update(int v, int l, int r) {
        push(v, l, r);
        if (v < one - 1) {
            push(2 * v + 1, l, (r + l) / 2);
            push(2 * v + 2, (r + l) / 2, r);
            left[v] = left[2 * v + 1];
            right[v] = right[2 * v + 2];
            kol[v] = (kol[2 * v + 2] + kol[2 * v + 1]);
            sum[v] = (sum[2 * v + 2] + sum[2 * v + 1]);
            if (right[2 * v + 1] && left[2 * v + 2]) {
                kol[v]--;
            }
        }
    }

}