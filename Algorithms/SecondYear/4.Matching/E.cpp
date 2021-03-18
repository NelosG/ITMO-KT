#include <iostream>
#include <vector>


using namespace std;


bool dfs(int v, vector<vector<int>> &g, vector<bool> &used, vector<int> &matching, vector<bool> &rev_matching) {
    if (used[v]) return false;
    used[v] = true;
    for (int i : g[v]) {
        if (matching[i] == -1 || dfs(matching[i], g, used, matching, rev_matching)) {
            matching[i] = v;
            rev_matching[v] = true;
            return true;
        }
    }
    return false;
}


int main() {
    int n, m, a, b, emptys = 0;
    cin >> n >> m >> a >> b;
    vector<vector<char>> board(n, vector<char>(m));
    vector<vector<int>> g(n * m);
    vector<int> matching;
    vector<bool> rev_matching, used;
    char ch;

    for (int i = 0; i < n; i++)
        for (int j = 0; j < m; j++)
            board[i][j] = (cin >> ch, ch) == '*';


    for (int i = 0; i < n; i++)
        for (int j = 0; j < m; j++)
            if (board[i][j]) {
                emptys++;
                if ((i + j + 1) % 2) {
                    int ind = i * m + j;
                    if (j && board[i][j - 1]) g[ind].push_back(ind - 1);
                    if ((j < m - 1) && board[i][j + 1]) g[ind].push_back(ind + 1);
                    if (i && board[i - 1][j]) g[ind].push_back(ind - m);
                    if ((i < n - 1) && board[i + 1][j]) g[ind].push_back(ind + m);
                }
            }


    if (2 * b <= a) {
        cout << emptys * b;
        return 0;
    }

    matching.assign(n * m, -1);
    rev_matching.assign(n * m, false);

    bool flag = true;
    while (flag) {
        flag = false;
        used.assign(n * m, false);
        for (int i = 0; i < n; i++)
            for (int j = 0; j < m; j++)
                flag |= (i + j + 1) % 2 && !rev_matching[i * m + j] && dfs(i * m + j, g, used, matching, rev_matching);
    }

    int c = 0;
    for (int i = 0; i < n * m; i++)
        if (matching[i] != -1)
            c++;

    cout << c * a + (emptys - 2 * c) * b;
    return 0;
}
