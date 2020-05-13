import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.util.*;

public class E {

    public static void main(String[] args) throws FileNotFoundException {
        Scanner scanner = new Scanner(new FileInputStream("problem5.in"));
        PrintWriter writer = new PrintWriter("problem5.out");
        int n = scanner.nextInt(), m = scanner.nextInt() , k = scanner.nextInt(), length = scanner.nextInt();
        HashSet<Integer> TerminalNKA = new HashSet<>();
        ArrayList<HashMap<Character, HashSet<Integer>>> NKA = new ArrayList<>();
        ArrayDeque<HashSet<Integer>> q = new ArrayDeque<>();
        ArrayList<HashSet<Integer>> conditions = new ArrayList<>();
        ArrayList<HashMap<Character, Integer>> DKA = new ArrayList<>();
        HashSet<Integer> TerminalDKA = new HashSet<>();
        for (int i = 0; i < k; i++) {
            TerminalNKA.add(scanner.nextInt());
        }
        for (int i = 0; i < n + 1; i++) {
            NKA.add(new HashMap<>());
        }
        int a, b;
        for (int i = 0; i < m; i++) {
            char ch;
            a = scanner.nextInt();
            b = scanner.nextInt();
            ch = scanner.next().charAt(0);
            if (!NKA.get(a).containsKey(ch)) {
                NKA.get(a).put(ch, new HashSet<>());
            }
            NKA.get(a).get(ch).add(b);
        }
        scanner.close();
        for (int i = 0; i < 1000; i++) {
            DKA.add(new HashMap<>());
            for (char ch = 'a'; ch <= 'z'; ch++) {
                DKA.get(i).put(ch, 0);
            }
        }
        HashSet<Integer> nextCondition;
        HashSet<Integer> condition = new HashSet<>();
        condition.add(1);
        q.add(condition);
        conditions.add(condition);

        while (!q.isEmpty()) {
            condition = q.pop();
            for (char ch = 'a'; ch <= 'z'; ch++) {
                nextCondition = new HashSet<>();

                for (int cur: condition)
                    if (NKA.size() > cur && NKA.get(cur).get(ch) != null)
                        nextCondition.addAll(NKA.get(cur).get(ch));

                if (!(conditions.contains(nextCondition) && nextCondition.isEmpty())) {
                    q.push(nextCondition);
                    conditions.add(nextCondition);
                }

                if (conditions.contains(nextCondition))
                    DKA.get(conditions.indexOf(condition) + 1).put(ch, conditions.indexOf(nextCondition) + 1);
            }
        }

        for (int i = 1; i <= conditions.size(); i++) {
            for (int set: conditions.get(i - 1)) {
                if (TerminalNKA.contains(set)) {
                    TerminalDKA.add(i);
                    break;
                }
            }
        }


        int mod = 1000000007;
        n = DKA.size();
        int[][] dop = new int[length + 1][n + 1];
        dop[0][1] = 1;
        int freq;
        for (int i = 1; i < length + 1; i++) {
            for (int j = 1; j < n; j++) {
                for (int p = 1; p < n; p++) {
                    freq = 0;
                    HashMap<Character,Integer> tp = DKA.get(p);
                    for (Map.Entry<Character,Integer> l : tp.entrySet())
                        if(l.getValue() == j) freq++;
                    for (int t = 0; t < freq; t++) {
                        dop[i][j] = (dop[i][j] + dop[i - 1][p]) % mod;
                    }
                }
            }
        }
        freq = 0;
        for (Integer t : TerminalDKA) {
            freq = (freq + dop[length][t]) % mod;
        }


        writer.print(freq);
        writer.close();
    }
}