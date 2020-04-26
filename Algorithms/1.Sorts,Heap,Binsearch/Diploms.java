import java.util.Scanner;

public class Diploms {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        long w = sc.nextLong();
        long h = sc.nextLong();
        long n = sc.nextLong();
        sc.close();
        long r;
        long l;

        if (w <= h) {
            l = w;
            r = n * h;
        } else {
            l = h;
            r = n * w;
        }

        while (l < r) {
            long mid = l + (r - l) / 2;
            if (n <= (mid / w) * (mid / h)) {
                r = mid;
            } else {
                l = mid + 1;
            }
        }
        System.out.println(l);

    }
}
