package ticTacToe;

public class Tournament {

    final Player[] a;
    static Size s;
    static int n = 0;
    static int k = 0;
    static int[] res;

    public Tournament() {
        int n1 = Read.read("How many games?    ",
                "Number of games not recognized" + "\n" + "Try again" + "\n", 1,  -1);
        this.n = n1;
        int k = Read.read("How many players?  ",
                "Number of players not recognized" + "\n" + "Try again" + "\n", 2, -1);
        this.a = new Player[k];
        res = new int[k];
        this.k = k;
        for (int i = 0; i < k; i++) {
            n1 = Read.read("1 - Random player" + "\n" + "2 - Sequential Player" + "\n" + "3 - Human Player" + "\n" + "Enter " + (i + 1) + " player   ",
                    "Id of player " + (i + 1) + " not recognized" + "\n" + "Try again" + "\n", 1, 3);
            if (n1 == 1) {
                a[i] = new RandomPlayer();
            } else {
                if (n1 == 2) {
                    a[i] = new SequentialPlayer();
                } else {
                    if (n1 == 3) {
                        a[i] = new HumanPlayer();
                    }
                }
            }
        }
    }

    public void play() {
        int temp = 0;
        int first;
        int second;
        s = new Size();
        for (int i = 0; i < k - 1; i++) {
            first = i;
            for (int j = i + 1; j < k; j++) {
                second = j;
                for (int s = 0; s < n; s++) {
                    if (s > 0) {
                        int tmp = first;
                        first = second;
                        second = tmp;
                    }
                    Game game = new Game(false, a[first], a[second]);

                    temp = game.play(first + 1, second + 1);

                    if (temp == 0) {
                        System.out.println("Draw");
                        res[first] += 1;
                        res[second] += 1;
                    } else {
                        System.out.println("Player  " + temp + "  win");
                        res[temp - 1] += 3;
                    }
                }
            }
        }
        StringBuilder sb = new StringBuilder();
        int[] winner = new int[2];
        for (int i = 0; i < res.length; i++) {
            if (res[i] > winner[1]) {
                winner[0] = i;
                winner[1] = res[i];
            }
            sb.append("Player ").append(i + 1).append(" score is ").append(res[i]).append("\n");
        }
        sb.append("\n\n").append("Player ").append((winner[0] + 1)).append(" Win with Score:  ").append(winner[1]);
        sb.append("\n\n").append("█████ █████ █  █ █████ █████ ██████ ███████ █    █ █     ██████  ███████  █████ █████ █  █ █████" + "\n" +
                                 "█   █ █   █ ██ █ █     █   █ █    █    █    █    █ █     █    █     █       █   █   █ ██ █ █" + "\n" +
                                 "█     █   █ █ ██ █  ██ █████ ██████    █    █    █ █     ██████     █       █   █   █ █ ██ █████" + "\n" +
                                 "█   █ █   █ █  █ █   █ █ █   █    █    █    █    █ █     █    █     █       █   █   █ █  █     █" + "\n" +
                                 "█████ █████ █  █ █████ █  █  █    █    █    ██████ █████ █    █     █     █████ █████ █  █ █████");

                System.out.println(sb.toString());
    }
}
