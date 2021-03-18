#include <algorithm>
#include <iostream>
#include <queue>
#include <unordered_set>
#include <vector>

using namespace std;

int main() {
    int s = 0, n, m;

    cin >> n >> m;
    vector<unordered_set<int>> G(n, unordered_set<int>());
    for (int i = 0, u, v; i < m; i++) {
        cin >> u >> v;
        --u, --v;
        G[u].insert(v);
        G[v].insert(u);
    }

    int Max = -1;
    for (auto &i : G) {
        Max = max(Max, (int) i.size());
    }
    Max = Max % 2 == 0 ? Max + 1 : Max;

    queue<int> q;
    q.push(s);
    vector<int> color(n, 0);
    color[s] = 1;

    while (!q.empty()) {
        int v = q.front();
        q.pop();
        for (auto &to : G[v]) {
            vector<bool> colors(Max + 1, false);
            if (color[to] == 0) {
                colors[color[v]] = true;
                for (auto &toto : G[to]) {
                    colors[color[toto]] = true;
                }
                int col = -1;
                for (int i = 1; i < colors.size(); i++) {
                    if (!colors[i]) {
                        col = i;
                        break;
                    }
                }
                color[to] = col;
                q.push(to);
            }
        }
    }

    cout << Max << '\n';
    for (auto &i : color) {
        cout << i << '\n';
    }
}
