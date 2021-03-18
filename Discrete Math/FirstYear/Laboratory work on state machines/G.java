import java.io.*;
import java.util.*;

public class G {

    public static void main(String[] args) throws IOException {
        FileReader in = new FileReader("equivalence.in");
        Scanner sc = new Scanner(in);
        PrintWriter out = new PrintWriter("equivalence.out");
        int a, b;
        char c;


        int n1 = sc.nextInt(), m1 = sc.nextInt(), k1 = sc.nextInt();
        int[][] arr1 = new int[n1 + 1][27];
        ArrayList<Integer> dop1 = new ArrayList<>();
        for (int i = 0; i < k1; i++) {
            dop1.add(sc.nextInt());
        }
        for (int i = 0; i < m1; i++) {
            a = sc.nextInt();
            b = sc.nextInt();
            c = sc.next().charAt(0);
            arr1[a][c - 'a' + 1]= b;
        }


        int n2 = sc.nextInt(), m2 = sc.nextInt(), k2 = sc.nextInt();
        int[][] arr2 = new int[n2 + 1][27];
        ArrayList<Integer> dop2 = new ArrayList<>();
        for (int i = 0; i < k2; i++) {
            dop2.add(sc.nextInt());
        }
        for (int i = 0; i < m2; i++) {
            a = sc.nextInt();
            b = sc.nextInt();
            c = sc.next().charAt(0);
            arr2[a][c - 'a' + 1]= b;
        }

        boolean[][] used = new boolean[n1 + 1][n2 + 1];
        Queue<Pair> q = new LinkedList<>();
        q.add(new Pair(1, 1));
        int u, v;
        boolean res = true;
        while (!q.isEmpty()) {
            Pair temp = q.poll();
            u = temp.a;
            v = temp.b;
            if (dop1.contains(u) != dop2.contains(v)) {
                res =  false;
                break;
            }
            used[u][v] = true;
            for (int i = 1; i < 27; i++) {
                if(!used[arr1[u][i]][arr2[v][i]]) {
                    q.add(new Pair(arr1[u][i], arr2[v][i]));
                }
            }
        }
        out.println(res ? "YES" : "NO");
        out.close();
    }

    static class Pair {
        int a;
        int b;

        Pair(int a, int b) {
            this.a = a;
            this.b = b;
        }
    }
}
