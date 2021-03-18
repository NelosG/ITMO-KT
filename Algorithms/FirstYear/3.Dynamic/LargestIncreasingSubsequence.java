import java.util.ArrayList;
import java.util.Scanner;

public class LargestIncreasingSubsequence {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        int n = sc.nextInt();
        int[] in = new int[n];
        for (int i = 0; i < n; i++) {
            in[i] = sc.nextInt();
        }
        ArrayList<Integer> out = find(in, n);
        StringBuilder sb = new StringBuilder();
        sb.append(out.size() + "\n");
        for (int i = 0; i < out.size(); i++) {
            sb.append(out.get(i) + " ");
        }
        System.out.println(sb.toString());
    }

    static ArrayList<Integer> find(int[] a, int n) {
        int[] prev = new int[n];
        int[] d = new int[n];

        for (int i = 0; i < n; i++) {
            d[i] = 1;
            prev[i] = -1;
            for (int j = 0; j < i; j++) {
                if (a[j] < a[i] && d[j] + 1 > d[i]) {
                    d[i] = d[j] + 1;
                    prev[i] = j;
                }
            }
        }


        int pos = 0;                            // индекс последнего элемента НВП
        int length = d[0];                      // длина НВП
        for (int i = 0; i < n; i++) {
            if (d[i] > length) {
                pos = i;
                length = d[i];
            }
        }

        // восстановление ответа
        ArrayList<Integer> answer = new ArrayList<>();
        while (pos != -1) {
            answer.add(0, a[pos]);
            pos = prev[pos];
        }

        return answer;
    }
}