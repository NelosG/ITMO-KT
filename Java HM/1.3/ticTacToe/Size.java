package ticTacToe;
public class Size {
    static int m,n,k, empt;
    Size() {
        System.out.println("Enter m, n, k");
        m = Read.read("Enter n    ",
                "Input err" + "\n" + "n not rcognized" + "\n" + "Try again" + "\n", 1, -1);
        n = Read.read("Enter m    ",
                "Input err" + "\n" + "m not rcognized" + "\n" + "Try again" + "\n", 1, -1);
        k = Read.read("Enter k    ",
                "Input err" + "\n" + "k not rcognized" + "\n" + "Try again" + "\n", 1, -1);
        this.empt = m * n;
    }
}
