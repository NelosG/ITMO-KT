import java.util.Scanner;

public class VeryEasyTask {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        int n = sc.nextInt();
        int x = sc.nextInt();
        int y = sc.nextInt();
        int time;
        int nx = 0;
        int ny = 0;
        int time1 = 0;
        if (x >= y) {
            time = y;
        } else {
            time = x;
        }
        n--;
        time += x * y * (n / (x + y));
        n = n % (x + y);
        while (nx + ny < n) {
            time1++;
            if (time1 % x == 0) {
                nx++;
            }
            if (time1 % y == 0) {
                ny++;
            }
        }
        time += time1;


        System.out.println(time);

    }
}