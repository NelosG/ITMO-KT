import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.Scanner;
 
public class BB {
 
    static String s;
    static HashSet<Integer>[][] arr;
    static ArrayList<Integer> last = new ArrayList<>();
 
    public static void main(String[] args) throws IOException {
        FileReader in = new FileReader("problem2.in");
        Scanner sc = new Scanner(in);
        FileWriter wr = new FileWriter("problem2.out");
        s = sc.next();
        int n = sc.nextInt(), m = sc.nextInt(), k = sc.nextInt();
        arr = new HashSet[n][26];
        int[] dop = new int[n];
        int l;
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < 26; j++) {
                arr[i][j] = new HashSet<>();
            }
        }
        for (int i = 0; i < k; i++) {
            l = sc.nextInt();
            dop[l - 1] = 1;
        }
        int a, b;
        char c;
        for (int i = 0; i < m; i++) {
            a = sc.nextInt();
            b = sc.nextInt();
            c = sc.next().charAt(0);
            arr[a - 1][c - 'a'].add(b - 1);
        }
        boolean flag = check(dop);
        if (!flag) {
            wr.write("Rejects");
        } else {
            wr.write("Accepts");
        }
        wr.close();
    }
 
    static boolean check(int[] dop) {
        HashSet<Integer> prev = arr[0][s.charAt(0) - 'a'];
        HashSet<Integer> now;
        for (int i = 1; i < s.length(); i++) {
            now = new HashSet<>();
            for (Integer p : prev) {
                for (Integer q :arr[p][s.charAt(i) - 'a']) {
                    now.add(q);
                }
            }
            prev = now;
        }
        for(Integer p : prev) {
            if(dop[p] == 1) return true;
        }
        return false;
    }

}
