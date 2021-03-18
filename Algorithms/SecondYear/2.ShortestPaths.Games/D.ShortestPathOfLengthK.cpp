#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int main() {
    vector<pair<pair<int, int>, int>> E;
    vector<vector<int>> dk;
    int n, m, k, s;
    cin >> n >> m >> k >> s;
    s--;
    dk.resize(k + 1, vector<int>(n, 2e9));
    for (int i = 0; i < m; ++i) {
        int u, v, w;;
        cin >> u >> v >> w;
        u--;
        v--;
        E.push_back({{u, v}, w});
    }
    dk[0][s] = 0;
    for (int i = 1; i <= k; ++i)
        for (auto e : E)
            if (dk[i][e.first.second] > dk[i - 1][e.first.first] + e.second)
                dk[i][e.first.second] = dk[i - 1][e.first.first] + e.second;
    for (int i = 0; i < n; ++i)
        cout << (dk[k][i] > 1e9 ? -1 : dk[k][i]) << '\n';
    return 0;
}
