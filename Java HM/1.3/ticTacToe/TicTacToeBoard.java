package ticTacToe;

import java.util.Arrays;
import java.util.Map;

public class TicTacToeBoard implements Board, Position{
    private static final Map<Cell, Character> SYMBOLS = Map.of(
            Cell.X, 'X',
            Cell.O, 'O',
            Cell.E, '.'
    );

    private final Cell[][] cells;
    private Cell turn;
    static int m = Tournament.s.m;
    static int n = Tournament.s.n;
    static int k = Tournament.s.k;
    static int empt = Tournament.s.empt;
    public int c;
    public int r;
    public TicTacToeBoard() {
        this.cells = new Cell[n][m];
        for (Cell[] row : cells) {
            Arrays.fill(row, Cell.E);
        }
        turn = Cell.X;
    }

    @Override
    public Position getPosition() {
        return this;
    }

    @Override
    public Cell getCell() {
        return turn;
    }

    @Override
    public Result makeMove(Move move) {
        this.r = move.row;
        this.c = move.column;
        empt--;
        if (empt < 0) {
            empt = 0;
        }
        if (!isValid(move)) {
            return Result.LOSE;
        }
        cells[move.getRow()][move.getColumn()] = move.getValue();
        for (int s = -1; s < 1; s++) {
            for (int d = -1; d <= (s * -1 != 0 ? s * -1 : -1); d++) {
                if (check(r + s, c + d, s, d) + 1 == k ||
                        check(r - s, c - d, s * -1, d * -1) + 1 == k) {
                    return Result.WIN;

                }
            }
        }
        if (empt == 0) {
            return Result.DRAW;
        }
        turn = turn == Cell.X ? Cell.O : Cell.X;
        return Result.UNKNOWN;
    }

    private int check(int currow, int curcolumn, int rowchange, int columnchange) {
        if (0 <= currow && currow < n &&
            0 <= curcolumn && curcolumn < m && cells[currow][curcolumn] == turn) {
            return check(currow + rowchange, curcolumn + columnchange, rowchange, columnchange) + 1;
        } else {
            return 0;
        }
    }

    @Override
    public boolean isValid(final Move move) {
        return 0 <= move.getRow() && move.getRow() < n
                && 0 <= move.getColumn() && move.getColumn() < m
                && cells[move.getRow()][move.getColumn()] == Cell.E
                && turn == getCell();
    }

    @Override
    public Cell getCell(final int r, final int c) {
        return cells[r][c];
    }

    @Override
    public String toString() {
        final StringBuilder sb = new StringBuilder("  ");
        for (int re = 0; re < m; re++) {
            sb.append(re);
        }
        for (int r = 0; r < n; r++) {
            sb.append("\n");
            sb.append(r);
            sb.append(" ");
            for (int c = 0; c < m; c++) {
                sb.append(SYMBOLS.get(cells[r][c]));
            }
        }
        return sb.toString();
    }
}
