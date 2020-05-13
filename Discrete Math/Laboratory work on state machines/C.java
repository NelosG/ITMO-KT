import java.io.FileInputStream;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.ArrayList;
import java.util.Scanner;

import static java.util.Collections.reverse;

public class C {
    static ArrayList<ArrayList<Integer>> aut, reverseAut;
    static int[] dop;
    static ArrayList<Integer> topSort = new ArrayList<>();
    static boolean[] usefull;
    static int[] cyc;
    static int n = 0;
    static int mod = 1000000007;

    static void dfsUseful(int v) {
        usefull[v] = true;
        int size = reverseAut.get(v).size();
        for (int i = 0; i < size; i++)
            if (!usefull[reverseAut.get(v).get(i)])
                dfsUseful(reverseAut.get(v).get(i));
    }

    static boolean dfsCycles(int v) {
        cyc[v] = 1;
        int size = aut.get(v).size();
        for (int i = 0; i < size; i++) {
            int cur = aut.get(v).get(i);
            if (cyc[cur] == 0) {
                if (dfsCycles(cur)) return true;
            } else if (usefull[cur] && cyc[cur] == 1) {
                return true;
            }
        }
        cyc[v] = 2;
        topSort.add(v);
        return false;
    }


    public static void main(String[] args) throws IOException {
        Scanner sc = new Scanner(new FileInputStream("problem3.in"));
        PrintWriter writer = new PrintWriter("problem3.out");
        n = sc.nextInt();
        int m = sc.nextInt(), k = sc.nextInt();
        dop = new int[k];
        usefull = new boolean[n];
        cyc = new int[n];
        aut = new ArrayList<>();
        reverseAut = new ArrayList<>();
        for (int i = 0; i < n; i++) {
            aut.add(new ArrayList<>());
            reverseAut.add(new ArrayList<>());
        }
        for (int i = 0, a; i < k; i++) {
            a = sc.nextInt();
            dop[i] = a - 1;
        }
        for (int i = 0, a, b; i < m; i++) {
            a = sc.nextInt();
            b = sc.nextInt();
            sc.next();
            aut.get(a - 1).add(b - 1);
            reverseAut.get(b - 1).add(a - 1);
        }
        int answer = -1;
        for (int i : dop)
            dfsUseful(i);
        if (!dfsCycles(0)) {
            reverse(topSort);
            answer = 0;
            int[] paths = new int[n];
            paths[0] = 1;
            int size = topSort.size();
            int cur;
            int sz;
            for (int i = 0; i < size; i++) {
                cur = topSort.get(i);
                sz = reverseAut.get(cur).size();
                for (int j = 0; j < sz; j++)
                    paths[cur] = (paths[cur] + paths[reverseAut.get(cur).get(j)]) % mod;
            }
            for (int i = 0; i < dop.length; i++)
                answer = (answer + paths[dop[i]]) % mod;
        }
        writer.print(answer % mod);
        writer.close();
    }

}