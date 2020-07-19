import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.PrintWriter;
import java.util.Scanner;
 
public class C {
    static Pair[] A = new Pair[7];
    static int res = Integer.MAX_VALUE;
 
    public static void main(String[] args) throws FileNotFoundException {
//        Scanner sc = new Scanner(System.in);
        Scanner sc = new Scanner(new FileReader("printing.in"));
        PrintWriter wr = new PrintWriter("printing.out");
        int n = sc.nextInt();
        int k = 1;
        for (int i = 0; i < 7; i++) {
            A[i] = new Pair(k, sc.nextInt());
            k *= 10;
        }
        end(0, n);
        check(0, n);
        wr.print(res);
        wr.close();
//        System.out.println(res);
    }
 
    static void check(int sum, int n) {
        if (n <= 0) {
            if (res > sum) res = sum;
            return;
        }
        int temp;
        for (int i = 0; i < 7; i++) {
            temp = n / A[i].kol * A[i].cost;
            if (temp != 0) {
                end(sum + temp, n - (n / A[i].kol) * A[i].kol);
                check(sum + temp, n - (n / A[i].kol) * A[i].kol);
            }
        }
        end(sum, n);
    }
 
    static void end(int sum, int n) {
        if (n <= 0) {
            if (res > sum) res = sum;
            return;
        }
        int min = Integer.MAX_VALUE;
        int temp;
        for (int i = 0; i < 7; i++) {
            temp = n / A[i].kol + (n % A[i].kol == 0 ? 0 : 1);
            if (min > temp * A[i].cost) {
                min = temp * A[i].cost;
            }
        }
        if (res > min + sum) res = sum + min;
    }
 
    static class Pair {
        int kol;
        int cost;
 
        Pair(int a, int b) {
            kol = a;
            cost = b;
        }
    }
}