import java.util.Scanner;

public class ApproximateBinarySearch {
    private static int lb(int[] a, int x) {
        int l = 0;
        int r = a.length - 1;
        while (l < r) {
            final int mid = (l + r) / 2;
            int midVal = a[mid];

            if (midVal <= x) {
                l = mid + 1;
            } else {
                r = mid;
            }
        }
        return r;
    }

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        int n = sc.nextInt();
        int k = sc.nextInt();
        int i;
        int[] a = new int[n];


        for (i = 0; i < n; i++) {
            a[i] = sc.nextInt();
        }


        for (i = 0; i < k; i++) {
            int t = sc.nextInt();
            int s1 = lb(a, t);
            int s = s1;
            if (s - 1 >= 0) {
                s--;
            }
            if (Math.abs(t - a[s]) <= Math.abs(t - a[s1])) {
                System.out.println(a[s]);
            } else {
                System.out.println(a[s1]);
            }

        }
    }
}