package ticTacToe;

public class Game {
    private final boolean log;
    private final Player player1, player2;

    public Game(final boolean log, final Player player1, final Player player2) {
        this.log = log;
        this.player1 = player1;
        this.player2 = player2;
    }

    public int play(int id1, int id2) {
        TicTacToeBoard board = new TicTacToeBoard();
        while (true) {
            final int result1 = move(board, player1, id1);
            if (result1 != -1) {
                board.empt = Tournament.s.empt;
                return result1;
            }
            final int result2 = move(board, player2, id2);
            if (result2 != -1) {
                board.empt = Tournament.s.empt;
                return result2;
            }
        }
    }

    private int move(final Board board, final Player player, final int no) {
        log("Position:\n" + board);
        Move move = player.move(board.getPosition(), board.getCell());
         Result result = board.makeMove(move);
        log("Position:\n" + board);
        if (result == Result.WIN) {
            log("Player " + no + " won");
            return no;
        } else if (result == Result.LOSE) {
            log("Player " + no + " lose");
            return 3 - no;
        } else if (result == Result.DRAW) {
            log("Draw");
            return 0;
        } else {
            return -1;
        }
    }

    private void log(final String message) {
        if (log) {
            System.out.println(message);
        }
    }
}
