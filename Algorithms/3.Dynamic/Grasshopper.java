import java.io.*;
import java.nio.charset.StandardCharsets;
import java.util.Scanner;

public class Grasshopper {
    public static void main(String[] args) throws IOException {
        Scanner sc = new Scanner(new InputStreamReader(new FileInputStream("input.txt"), StandardCharsets.UTF_8));
        int N = sc.nextInt();
        int K = sc.nextInt();
        int[] in = new int[N + 1];
        String[] str = new String[N + 1];
        in[0] = 0;
        in[1] = 0;
        str[0] = "0";
        str[1] = "1";
        for (int i = 2; i < N; i++) {
            in[i] = sc.nextInt();
        }
        in[N] = 0;
        int[][] res = new int[N + 1][2];
        int j1;
        for (int i = 2; i < res.length; i++) {
            j1 = -1;
            int max = Integer.MIN_VALUE;
            for (int j = i - 1; j >= i - K; j--) {
                if (j < 1) {
                    break;
                }
                if (res[j][0] > max) {
                    max = res[j][0];
                    j1 = j;
                }
            }
            str[i] = str[j1] + " " + i;
            res[i][0] = max + in[i];
            res[i][1] = res[j1][1] + 1;
        }

        //System.out.println(max + "\n" + (res[i1][1] + 1) + "\n" + str[i1] + " " + N);
        BufferedWriter out = new BufferedWriter(new OutputStreamWriter(new FileOutputStream("output.txt"), StandardCharsets.UTF_8));
        out.write(res[res.length - 1][0] + "\n" + (res[res.length - 1][1]) + "\n" + str[res.length - 1]);
        out.close();
    }
}