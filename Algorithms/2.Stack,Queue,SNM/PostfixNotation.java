import java.util.ArrayList;
import java.util.Scanner;

public class PostfixNotation {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        String s = sc.nextLine() + " ";
        sc.close();
        ArrayList<Integer> a = new ArrayList<>();
        int i = 0;
        while (i < s.length()) {
            while (i < s.length() && Character.isDigit(s.charAt(i))) {
                int j = i;
                while (i < s.length() && Character.isDigit(s.charAt(i))) {
                    i++;
                }
                a.add(Integer.parseInt(s.substring(j, i)));
                i++;
            }
            while (i < s.length() && !Character.isDigit(s.charAt(i))) {
                int x = a.get(a.size() - 2);
                int y = a.get(a.size() - 1);
                int d = 0;
                if (s.charAt(i) == '+') {
                    d = x + y;
                } else {
                    if (s.charAt(i) == '-') {
                        d = x - y;

                    } else {
                        if (s.charAt(i) == '*') {
                            d = x * y;
                        }
                    }
                }
                i = i + 2;
                a.remove(a.size() - 1);
                a.remove(a.size() - 1);
                a.add(d);
            }
        }
        System.out.print(a.get(0));
    }
}