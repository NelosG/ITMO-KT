import java.util.Scanner;

public class Heap {
    public static void siftup(int[] heap, int i) {
        while ((i != 0) && (heap[i] > heap[(i - 1) / 2])) {
            int temp;
            temp = heap[i];
            heap[i] = heap[(i - 1) / 2];
            heap[(i - 1) / 2] = temp;
            i = (i - 1) / 2;
        }
    }

    public static void siftdown(int[] heap, int i) {
        while (2 * i + 2 < heap.length) {
            if (heap[2 * i + 2] > heap[2 * i + 1] && heap[2 * i + 2] > heap[i]) {
                int temp;
                temp = heap[i];
                heap[i] = heap[2 * i + 2];
                heap[2 * i + 2] = temp;
                i = 2 * i + 2;
            } else {
                if (heap[2 * i + 1] > heap[i]) {
                    int temp;
                    temp = heap[i];
                    heap[i] = heap[2 * i + 1];
                    heap[2 * i + 1] = temp;
                    i = 2 * i + 1;
                } else {
                    break;
                }
            }
        }
    }

    public static void main(String[] args) {
        int n = 0;
        Scanner sc = new Scanner(System.in);
        n = sc.nextInt();
        int[] heap = new int[n];
        for (int i = 0; i < n; i++) {
            int nn = sc.nextInt();
            if (nn == 0) {
                heap[i] = sc.nextInt();
                siftup(heap, i);
            } else {
                System.out.println(heap[0]);
                heap[0] = heap[n - 1];
                heap[n - 1] = 0;
                siftdown(heap, 0);
            }
        }
    }
}