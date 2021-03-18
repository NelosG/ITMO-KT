

#include <vector>
#include <iostream>
#include <algorithm>
#include <unordered_set>
#include <fstream>
#include <queue>

using namespace std;

int main() {
    std::ifstream in("game.in");

    int n, m, S;
    in >> n >> m >> S;
    vector<vector<int>> E(n + 1, vector<int>());
    vector<int> counter(n + 1, 0);
    for (int i = 0; i < m; i++) {
        int u, v;
        in >> u >> v;
        ++counter[v];
        E[u].emplace_back(v);
    }
    queue<int> q;
    for (int i = 1; i <= n; i++) {
        if (counter[i] == 0)
            q.push(i);
    }
    vector<int> topSort(n);
    for (int i = n - 1; i >= 0; --i) {
        if (q.empty())
            break;
        int v = q.front();
        q.pop();
        for (int u : E[v]) {
            counter[u]--;
            if (counter[u] == 0)
                q.push(u);
        }
        topSort[i] = v;
    }
    vector<bool> win(n + 1, false);
    for (int v : topSort)
        for (int u : E[v])
            if (win[u] == 0) {
                win[v] = true;
                break;
            }

    std::ofstream out("game.out");
    out << (win[S] ? "First player wins" : "Second player wins");
}
