import java.util.Arrays;
import java.util.Scanner;

public class NumberOfInversions {

    private static long cot;

    private static int[] sort(int[] arr) {
        if (arr.length < 2) {
            return arr;
        }
        int[] arr1 = Arrays.copyOfRange(arr, 0, arr.length / 2);
        int[] arr2 = Arrays.copyOfRange(arr, arr.length / 2, arr.length);
        arr1 = sort(arr1);
        arr2 = sort(arr2);
        return (mrg(arr1, arr2));
    }


    private static int[] mrg(int[] arr1, int[] arr2) {
        int n = arr1.length;
        int m = arr2.length;
        int[] res = new int[n + m];
        int i = 0;
        int j = 0;
        int s = 0;
        while (i < n || j < m) {
            if (i < n && j < m) {
                if (arr1[i] < arr2[j]) {
                    res[s++] = arr1[i++];
                } else {
                    res[s++] = arr2[j++];
                    cot += n - i;
                }
            } else if (i < n) {
                res[s++] = arr1[i++];
            } else if (j < m) {
                res[s++] = arr2[j++];
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
        a = sort(a);
        System.out.println(cot);
    }
}