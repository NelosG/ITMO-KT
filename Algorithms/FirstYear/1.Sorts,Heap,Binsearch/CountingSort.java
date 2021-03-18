import java.util.Scanner;

public class CountingSort {

    private static int[] sort(int[] ar, int f) {
        int[] b = new int[101];
        for (int i = 0; i < ar.length; i++) {
            b[ar[i]] += 1;
        }
        for (int i = 0; i < f; i++) {
            ar[i] = 0;
        }
        int c = f;
        for (int j = 1; j < 101; j++) {
            for (int i = 0; i < b[j]; i++) {
                ar[c] = j;
                c++;
            }
        }
        return ar;
    }

    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        int[] a = new int[200_001];
        int i = 0;
        int f = 0;
        while (sc.hasNextInt()) {
            a[i] = sc.nextInt();
            if (a[i] == 0) {
                f++;
            }
            i++;
        }
        sc.close();
        a = sort(a, f);
        for (int j = 0; j < i; j++) {
            System.out.print(a[j] + " ");
        }
    }
}
