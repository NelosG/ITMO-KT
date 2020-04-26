import java.util.Scanner;

public class SquareRootAndSquareSquare {
    public static void main(String[] args) {
        double i;
        Scanner sc = new Scanner(System.in);
        i = sc.nextDouble();
        double x;
        double f = i;
        double e = 0;
        double s = f;
        while (s > 0.000_001) {
            x = (e + f) / 2;
            if (x * x + Math.sqrt(x) < i) {
                e = x;
            } else {
                f = x;
            }
            s = f - e;
        }
        System.out.println(String.format("%.6f", (e + f) / 2));
    }
}
