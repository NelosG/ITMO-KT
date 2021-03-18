

#include <vector>
#include <iostream>
#include <algorithm>
#include <unordered_set>
#include <set>
#include <queue>

using namespace std;

int main() {
    int n, m;
    cin >> n >> m;
    vector<vector<int>> E(n + 1, vector<int>());
    vector<int> counter(n + 1, 0);
    queue<int> q;
    vector<int> top_sort(n);
    vector<int> grandi(n + 1);

    for (int i = 0; i < m; i++) {
        int u, v;
        cin >> u >> v;
        ++counter[v];
        E[u].emplace_back(v);
    }

    for (int i = 0; i < n; i++)
        if (counter[i + 1] == 0)
            q.push(i + 1);

    for (int i = n - 1; i >= 0; i--) {
        if (q.empty())
            break;
        int v = q.front();
        q.pop();
        for (int u : E[v]) {
            counter[u]--;
            if (counter[u] == 0)
                q.push(u);
        }
        top_sort[i] = v;
    }
    for (auto v : top_sort) {
        set<int> grandieSet;
        for (int u : E[v])
            grandieSet.insert(grandi[u]);
        grandi[v] = -1;
        int i = 0;
        for (int w : grandieSet) {
            if (i != w) {
                grandi[v] = i;
                break;
            }
            i++;
        }
        if (grandi[v] == -1) grandi[v] = i;
    }
    auto i = grandi.begin();
    ++i;
    for (; i != grandi.end(); ++i)
        cout << *i << "\n";
}
