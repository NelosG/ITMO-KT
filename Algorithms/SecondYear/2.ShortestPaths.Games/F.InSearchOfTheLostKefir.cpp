#include <vector>
#include <unordered_map>
#include <set>
#include <iostream>
#include <algorithm>

using namespace std;
using ll =  long long int;
using namespace std;

ll INF = 9223372036854775807ll;
int n;
vector<ll> d;
unordered_map<int, vector<pair<int, ll>>> E;

void deikstra(int start) {
    for (int i = 1; i <= n; i++)
        d[i] = INF;
    d[start] = 0;
    set<pair<ll, int>> set;
    set.insert({0, start});
    while (!set.empty()) {
        int min_v = set.begin()->second;
        ll min_w = set.begin()->first;
        set.erase(set.begin());
        if (min_w > d[min_v])
            continue;
        for (auto &pair : E[min_v]) {
            int u = pair.first;
            ll w = pair.second;
            if (d[u] > d[min_v] + w) {
                set.erase({u, d[u]});
                d[u] = d[min_v] + w;
                set.insert({d[u], u});
            }
        }
    }
}

int main() {
    ios_base::sync_with_stdio(false);
    int a, b, c, m;
    cin >> n >> m;
    d.resize(n + 1);
    ll w;
    for (int i = 0, u, v; i < m; i++) {
        cin >> u >> v >> w;
        E[u].push_back({v, w});
        E[v].push_back({u, w});
    }
    cin >> a >> b >> c;
    deikstra(a);
    ll ab = d[b];
    ll ac = d[c];
    deikstra(b);
    ll bc = d[c];
    if (ab == INF || ac == INF || bc == INF) cout << -1;
    else cout << min(ab + bc, min(bc + ac, ab + ac));
}
