import java.util.Scanner;

public class GladeOfFirewood {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        double vp = sc.nextDouble();
        double vf = sc.nextDouble();
        double a = sc.nextDouble();
        double x = 0;
        double min = (vp + vf) * 2;
        double s;
        double x1 = 0;
        while (x <= 1) {
            s = Math.sqrt((1 - a) * (1 - a) + x * x) / vp + Math.sqrt(a * a + (1 - x) * (1 - x)) / vf;
            if (s < min) {
                min = s;
                x1 = x;
            }
            x += 0.0001;
        }
        System.out.println(x1);
    }
}