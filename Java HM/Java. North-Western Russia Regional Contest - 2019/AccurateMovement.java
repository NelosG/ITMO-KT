import java.util.Scanner;
 
public class AccurateMovement {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        int a = sc.nextInt();
        int b = sc.nextInt();
        int n = sc.nextInt();
        int res = 0;
        if ((n - b)% (b - a) == 0) {
            res = ((n - b) / (b - a)) * 2 + 1;
        } else {
            res = ((n - b) / (b - a)) * 2 + 3;
        }
        System.out.println(res);
    }
}