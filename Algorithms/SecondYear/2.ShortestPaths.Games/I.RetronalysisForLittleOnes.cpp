#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

vector<vector<int>> RE, E;
vector<int> e;
vector<char> win;
vector<bool> used;

void dfs(int v) {
    used[v] = true;
    for (int i : RE[v]) {
        if (!used[i]) {
            if (win[v] == -1) {
                win[i] = 1;
                dfs(i);
            } else {
                e[i]++;
                if (E[i].size() - e[i] == 0) {
                    win[i] = -1;
                    dfs(i);
                }
            }
        }
    }
}

int main() {
    int n, m;
    while (cin >> n >> m) {
        E.clear();
        RE.clear();
        used.clear();
        win.clear();
        e.clear();
        RE.resize(n);
        E.resize(n);
        e.resize(n, 0);
        used.resize(n, false);
        win.resize(n, 0);
        for (int i = 0, u, v; i < m; ++i) {
            cin >> u >> v;
            u--;
            v--;
            E[u].push_back(v);
            RE[v].push_back(u);
        }
        for (int i = 0; i < n; ++i)
            if (E[i].empty()) {
                win[i] = -1;
                dfs(i);
            }

        for (int i = 0; i < n; ++i) {
            if (win[i] == 0) {
                cout << "DRAW\n";
                continue;
            }
            if (win[i] == 1) {
                cout << "FIRST\n";
                continue;
            }
            cout << "SECOND\n";
        }
        cout << '\n';
    }
    return 0;
}
