import java.util.ArrayList;
import java.util.Scanner;

public class Balls {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        int n = sc.nextInt();
        ArrayList ar = new ArrayList<>();
        int s = 0;
        int j1 = 0;
        int sum = 0;
        int i = 0;
        if (n > 2) {
            for (i = 0; i < n; i++) {
                ar.add(sc.nextInt());
            }
            i = 0;
            while (true) {
                s = 0;
                for (i = 0; i < n; i++) {
                    if (i > 1) {
                        if (ar.get(i) == ar.get(i - 1) && ar.get(i - 1) == ar.get(i - 2)) {
                            if (s != 0) {
                                s++;
                            } else {
                                s = 3;
                                j1 = i - 2;
                            }
                        }
                    }
                }
                if (s != 0) {
                    sum += s;
                    n = n - s;
                    for (int j = 0; j < s; j++) {
                        ar.remove(j1);
                    }
                } else {
                    break;
                }
            }
        }
        System.out.println(sum);
    }
}