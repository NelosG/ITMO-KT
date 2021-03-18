import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.util.Scanner;
import java.util.TreeSet;

public class Parking {
    static TreeSet<Integer> set = new TreeSet<>();

    public static void main(String[] args) throws IOException {
        FileReader in = new FileReader("parking.in");
        Scanner sc = new Scanner(in);
        FileWriter wr = new FileWriter("parking.out");
        int n = sc.nextInt();
        int m = sc.nextInt();
        for (int i = 1; i <= n; i++) {
            set.add(i);
        }
        for (int i = 0; i < m; i++) {
            String s = sc.next();
            int x = sc.nextInt();
            if (s.equals("enter")) {
                int temp = 0;
                if (set.contains(x)) {
                    temp = x;
                } else {
                    if (set.higher(x) == null) {
                        temp = set.first();
                    } else {
                        temp = set.higher(x);
                    }
                }
                wr.write(temp + "\n");
                set.remove(temp);
            } else {
                set.add(x);
            }
        }
        wr.close();
    }

    static int min(int a, int b) {
        if (a < b) return a;
        return b;
    }
}