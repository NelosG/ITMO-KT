import java.util.Scanner;
 
public class BadTreap {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        int n = sc.nextInt();
        StringBuilder s = new StringBuilder();
        for (int i = -25000; i < n - 25000; i++) {
            s.append((710 * i) + "\n");
        }
        System.out.println(s);
    }
}