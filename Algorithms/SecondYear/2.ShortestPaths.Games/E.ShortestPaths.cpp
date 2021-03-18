#include <vector>
#include <iostream>
#include <algorithm>

using namespace std;
using ll = long long int;

const ll INF = 10000000000000000;

vector<vector<pair<int, ll>>> E;
vector<bool> used;


void dfs(int v) {
    used[v] = true;
    for (auto pair : E[v])
        if (!used[pair.first])
            dfs(pair.first);
}


int main() {
    int n, m, s;
    cin >> n >> m >> s;

    E.resize(n + 1, vector<pair<int, ll>>());
    used.resize(n + 1, false);
    vector<ll> d(n + 1, INF);
    vector<int> p(n + 1, -1);


    for (int i = 0; i < m; i++) {
        int u, v;
        ll w;
        cin >> u >> v >> w;
        E[u].push_back({v, w});
    }

    d[s] = 0;
    for (int i = 1; i <= n; i++)
        for (int j = 1; j <= n; j++)
            for (pair<int, ll> pair : E[j]) {
                if ((d[j] < INF) && (d[pair.first] > d[j] + pair.second)) {
                    d[pair.first] = d[j] + pair.second;
                    p[pair.first] = j;
                }
            }

    for (int i = 1; i <= n; i++)
        for (pair<int, ll> pair : E[i])
            if ((d[i] < INF) &&
                        (d[pair.first] > d[i] + pair.second)
                            && !used[pair.first])
                dfs(i);

    for (int i = 1; i <= n; i++) {
        if (used[i]) cout << "-\n";
        else if (d[i] == INF) cout << "*\n";
        else cout << d[i] << "\n";
    }
}
