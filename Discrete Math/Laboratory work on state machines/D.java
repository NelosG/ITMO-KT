import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Scanner;

public class D {
    public static void main(String[] args) throws IOException {
        FileReader in = new FileReader("problem4.in");
        Scanner sc = new Scanner(in);
        PrintWriter out = new PrintWriter("problem4.out");
        int n = sc.nextInt(), m = sc.nextInt(), k = sc.nextInt(), l = sc.nextInt();
        ArrayList<Integer> term = new ArrayList<>();
        for (int i = 0; i < k; i++)
            term.add(sc.nextInt());
        ArrayList<ArrayList<Integer>> arr = new ArrayList<>(n);
        for (int i = 0; i < n + 1; i++)
            arr.add(new ArrayList<>(n));
        for (int i = 0; i < m; i++) {
            arr.get(sc.nextInt()).add(sc.nextInt());
            sc.next();
        }
        int mod = 1000000007;
        int[][] dop = new int[l + 1][n + 1];
        dop[0][1] = 1;
        int freq;
        for (int i = 1; i < l + 1; i++) {
            for (int j = 1; j < n + 1; j++) {
                for (int p = 1; p < n + 1; p++) {
                    freq = Collections.frequency(arr.get(p), j);
                    for (int q = 0; q < freq; q++) {
                        dop[i][j] = (dop[i][j] + dop[i - 1][p]) % mod;
                    }
                }
            }
        }
        freq = 0;
        for (Integer t : term) {
            freq = (freq + dop[l][t]) % mod;
        }
        out.print(freq % mod);
        out.close();
    }
}
