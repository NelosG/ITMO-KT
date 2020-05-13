import java.io.*;
import java.util.*;

public class JusttheLastDigit {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        int n = sc.nextInt();
        sc.nextLine();
        int[][] arr = new int[n][n];
        int[][] res = new int[n][n];
        for (int i = 0; i < n; i++) {
            String str = sc.nextLine();
            for (int j = 0; j < str.length(); j++) {
                arr[i][j] = Character.getNumericValue(str.charAt(j));
            }
        }

        for (int i = 0; i < n; i++) {
            for (int j = i + 1; j < n; j++) {
                if (arr[i][j] > 0) {
                    res[i][j] = 1;
                    for (int k = j + 1; k < n; k++)
                        arr[i][k] = (arr[i][k] - arr[j][k] + 10) % 10;
                }
            }
        }
        StringBuilder s = new StringBuilder();
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < n; j++)
                s.append(res[i][j]);
            s.append("\n");
        }
        System.out.println(s);
    }
}