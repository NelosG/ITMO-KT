import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.PrintWriter;
import java.util.Arrays;
import java.util.Scanner;
 
public class A {
    public static void main(String[] args) throws FileNotFoundException {
        Scanner sc = new Scanner(new FileReader("cobbler.in"));
        PrintWriter wr = new PrintWriter("cobbler.out");
//        Scanner sc = new Scanner(System.in);
        int k = sc.nextInt(), n = sc.nextInt();
        int[] arr = new int[n];
        for (int i = 0; i < n; i++) {
            arr[i] = sc.nextInt();
        }
 
        Arrays.sort(arr);
        int i = 0;
        while (i < arr.length && k - arr[i] >= 0) {
            k -= arr[i];
            i++;
        }
//        System.out.println(i);
        wr.print(i);
        wr.close();
    }
}