import java.util.Arrays;
import java.util.Scanner;

public class FastSearchInArray {
    private static int sch(int[] a, int l, int r) {
        int k = 0;
        r++;
        l = lb(a, l);
        k = ub(a, r) - l;
        return k;
    }

    private static int lb(int[] a, int x) {
        int l = -1;
        int r = a.length;
        while (l < r - 1) {
            int mid = (l + r) >>> 1;
            int midVal = a[mid];

            if (midVal < x) {
                l = mid;
            } else {
                r = mid;
            }
        }
        return r;
    }

    private static int ub(int[] a, int x) {
        int l = -1;
        int r = a.length;
        while (l < r - 1) {
            int mid = (l + r) >>> 1;
            int midVal = a[mid];

            if (midVal < x) {
                l = mid;
            } else {
                r = mid;
            }
        }
        return r;
    }

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        int n = sc.nextInt();
        int i = 0;
        int[] a = new int[n];
        for (i = 0; i < n; i++) {
            a[i] = sc.nextInt();
        }
        Arrays.sort(a);
        int k = sc.nextInt();
        for (i = 0; i < k; i++) {
            int l = sc.nextInt();
            int r = sc.nextInt();
            System.out.print(sch(a, l, r) + " ");

        }
    }
}