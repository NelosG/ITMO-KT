import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Scanner;

public class F {

    static int[][] arr1;
    static int[][] arr2;
    static int[] dop1;
    static int[] dop2;
    public static void main(String[] args) throws IOException {
        FileReader in = new FileReader("isomorphism.in");
        Scanner sc = new Scanner(in);
        FileWriter wr = new FileWriter("isomorphism.out");

        int n1 = sc.nextInt(), m1 = sc.nextInt(), k1 = sc.nextInt();
        arr1 = new int[n1][26];
        dop1 = new int[n1];
        int l;
        for (int i = 0; i < k1; i++) {
            l = sc.nextInt();
            dop1[l - 1] = 1;
        }
        int a, b;
        char c;
        for (int i = 0; i < m1; i++) {
            a = sc.nextInt();
            b = sc.nextInt();
            c = sc.next().charAt(0);
            arr1[a - 1][c - 'a']= b - 1;
        }


        int n2 = sc.nextInt(), m2 = sc.nextInt(), k2 = sc.nextInt();
        arr2 = new int[n2][26];
        dop2 = new int[n2];
        for (int i = 0; i < k2; i++) {
            l = sc.nextInt();
            dop2[l - 1] = 1;
        }
        for (int i = 0; i < m2; i++) {
            a = sc.nextInt();
            b = sc.nextInt();
            c = sc.next().charAt(0);
            arr2[a - 1][c - 'a']= b - 1;
        }
        boolean flag = false;
        visited = new boolean[n1];
        associations = new int[n1];
        if (n1 == n2) {
            flag = check(0, 0);
        }
        if (flag) {
            wr.write("YES");
        } else {
            wr.write("NO");
        }
        wr.close();
    }
    static boolean[] visited;
    static int[] associations;
    static boolean check(int u, int v) {
        visited[u] = true;
        associations[u] = v;
        boolean result = true;
        if(dop1[u] != dop2[v]) return false;
        for (int i = 0; i < 26; i++) {
            int t1 = arr1[u][i];
            int t2 = arr2[v][i];
            if ((t1 == -1 || t2 == -1) && t1 != t2) return false;
            if(visited[t1]) {
                result = result && (t2 == associations[t1]);
            } else {
                result = result && check(t1, t2);
            }
        }
        return result;
    }

}
