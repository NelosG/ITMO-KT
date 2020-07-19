import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.PrintWriter;
import java.util.HashSet;
import java.util.Scanner;
 
public class B {
    static HashSet<Pair> set;
 
    public static void main(String[] args) throws FileNotFoundException {
        Scanner sc = new Scanner(new FileReader("request.in"));
        PrintWriter wr = new PrintWriter("request.out");
//        Scanner sc = new Scanner(System.in);
        int n = sc.nextInt();
        set = new HashSet<>(n);
        for (int i = 0; i < n; i++) {
            set.add(new Pair(sc.nextInt(), sc.nextInt()));
        }
        Pair temp;
        int res = 0;
        while(!set.isEmpty()) {
            temp = new Pair(0, 2000);
            for (Pair i : set) {
                if(i.end < temp.end) temp = i;
            }
            set.remove(temp);
            while(delete(temp));
            res++;
        }
//        System.out.println(res);
        wr.print(res);
        wr.close();
    }
    static boolean delete(Pair temp) {
        for(Pair i : set) {
            if(i.start < temp.end)  {
                set.remove(i);
                return true;
            }
        }
        return false;
    }
    static class Pair {
        int start = 0;
        int end = 0;
        Pair(int a, int b) {
            start = a;
            end = b;
        }
    }
 
}