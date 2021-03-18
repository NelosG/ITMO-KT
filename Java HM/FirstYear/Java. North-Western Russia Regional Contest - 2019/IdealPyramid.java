import java.util.Scanner;

public class IdealPyramid {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        int n = sc.nextInt();
        int xl = Integer.MAX_VALUE;
        int xr = Integer.MIN_VALUE;
        int yl = Integer.MAX_VALUE;
        int yr = Integer.MIN_VALUE;
        int x = 0;
        int y = 0;
        int h = 0;
        for (int i = 0; i < n; i++) {
            x = sc.nextInt();
            y = sc.nextInt();
            h = sc.nextInt();
            if (x - h < xl)
                xl = x - h;
            if (y - h < yl)
                yl = y - h;
            if (x + h > xr)
                xr = x + h;
            if (y + h > yr)
                yr = y + h;
        }
        if (xr - xl > yr - yl)
            h = xr - xl;
        else
            h = yr - yl;
        if (h % 2 == 0)
            h = h / 2;
        else
            h = h / 2 + 1;

        x = (xl + xr) / 2;
        y = (yl + yr) / 2;
        System.out.println(x + " " + y + " " + h);
    }
}