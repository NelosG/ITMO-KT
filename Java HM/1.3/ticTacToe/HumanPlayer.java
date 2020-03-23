package ticTacToe;

import java.io.PrintStream;
import java.util.Scanner;

import ticTacToe.TicTacToeBoard;

public class HumanPlayer implements Player {
    private final PrintStream out;
    public int nowr;
    public int nowc;

    public HumanPlayer(final PrintStream out) {
        this.out = out;
    }

    public HumanPlayer() {
        this(System.out);
    }

    @Override
    public Move move(final Position position, final Cell cell) {
        while (true) {
            out.println(position);
            out.print("Enter row and column   ");
            nowr = Read.read("Enter row   ", "Enter number   ", 0, -1);
            nowc = Read.read("Enter column   ", "Enter number   ", 0, -1);
            final Move move = new Move(nowr, nowc, cell);
            if (position.isValid(move)) {
                return move;
            }

            out.println("Invalid move");
        }
    }
}