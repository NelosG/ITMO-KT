import java.util.Arrays;
import java.util.Scanner;

public class EasySort {
    private static int[] sort(int[] arr) {
        if (arr.length < 2) {
            return arr;
        }

        int[] arr1 = Arrays.copyOfRange(arr, 0, arr.length / 2);

        int[] arr2 = Arrays.copyOfRange(arr, (arr.length / 2), arr.length);

        arr1 = sort(arr1);
        arr2 = sort(arr2);

        return mrg(arr1, arr2);
    }


    private static int[] mrg(int[] ar1, int[] ar2) {
        int k = 0;
        int m = 0;
        int[] res = new int[ar1.length + ar2.length];
        while ((k + m) < res.length) {
            if (m == ar2.length || (k < ar1.length && ar1[k] < ar2[m])) {
                res[k + m] = ar1[k];
                k++;
            } else {
                res[k + m] = ar2[m];
                m++;
            }
        }
        return res;
    }


    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        int y = sc.nextInt();
        int[] a = new int[y];
        int i = 0;
        while (sc.hasNextInt()) {
            a[i] = sc.nextInt();
            i++;
        }
        sc.close();

        int[] k = sort(a);

        for (int n = 0; n < a.length; n++) {
            System.out.print(k[n] + " ");
        }

    }
}