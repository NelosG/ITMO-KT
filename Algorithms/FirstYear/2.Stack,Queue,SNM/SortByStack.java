import java.util.ArrayList;
import java.util.Scanner;

public class SortByStack {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        int n = sc.nextInt();
        boolean flag = true;
        int temp, i;
        StringBuilder s = new StringBuilder();
        ArrayList<Integer> a = new ArrayList<>();
        ArrayList<Integer> ar = new ArrayList<>();
        while (sc.hasNextInt()) {
            temp = sc.nextInt();
            i = ar.size() - 1;
            while (i >= 0 && ar.get(i) < temp) {
                s.append("pop" + "\n");
                a.add(ar.get(i));
                ar.remove(i);
                i--;
            }
            s.append("push" + "\n");
            ar.add(temp);
        }

        i = ar.size() - 1;
        while (ar.size() != 0) {
            s.append("pop" + "\n"); //
            a.add(ar.get(i));
            ar.remove(i);
            i--;
        }


        for (i = 1; i < a.size(); i++) {
            if (a.get(i - 1) > a.get(i)) {
                flag = false;
                break;
            }
        }

        if (flag) {
            System.out.println(s.toString());
        } else {
            System.out.println("impossible");
        }
    }
}
