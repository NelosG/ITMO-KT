package calc;

import java.util.Scanner;

public class TestCalcParser {
    public static void main(String[] args) {
        Scanner sc = new Scanner(System.in);
        while (sc.hasNextLine()) {
            System.out.println(new CalcParser(new CalcLexer(sc.nextLine())).parse());
        }
    }
}
