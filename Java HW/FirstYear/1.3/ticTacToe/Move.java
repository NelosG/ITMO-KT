package ticTacToe;

public final class Move {
    public final int row;
    public final int column;
    public final Cell value;

    public Move(final int row, final int column, final Cell value) {
        this.row = row;
        this.column = column;
        this.value = value;
    }

    public int getRow() {
        return row;
    }

    public int getColumn() {
        return column;
    }

    public Cell getValue() {
        return value;
    }

    @Override
    public String toString() {
        return "row=" + row + ", column=" + column + ", value=" + value;
    }
}
