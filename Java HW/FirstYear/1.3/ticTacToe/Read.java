package ticTacToe;

import java.util.InputMismatchException;
import java.util.Scanner;

public class Read {

    static int read(String out, String err, int begin, int end) {
        int res = 0;
        boolean flag = true;
        while (flag) {
            Scanner sc = new Scanner(System.in);
            flag = false;
            System.out.println();
            try {
                System.out.print(out);
                res = sc.nextInt();
                if (res < begin || end >= 0 && res > end) {
                    throw new InputMismatchException();
                }
            } catch (Exception e) {
                flag = true;
                System.err.println(err);
            }
        }
        return res;
    }
}
