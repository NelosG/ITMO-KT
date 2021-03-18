#include <iostream>
#include <vector>

using namespace std;

vector<vector<pair<int, int>>> E;
vector<bool> used;
vector<int> g;
int ans = -1;

void grundy(int u) {
    used[u] = true;
    g[u] = 0;
    for (auto i : E[u]) {
        if (!used[i.first]) {
            grundy(i.first);
            g[u] ^= (g[i.first] + 1);
        }
    }
}

void dfs(int u, int need) {
    used[u] = true;
    for (auto i : E[u]) {
        if (!used[i.first]) {
            int new_need = need ^g[u] ^(g[i.first] + 1);
            if (new_need == 0) {
                ans = i.second;
                return;
            }
            dfs(i.first, new_need - 1);
            if (ans != -1)
                return;
        }
    }
}


int main() {
    ios_base::sync_with_stdio(false);
    int n, r;
    cin >> n >> r;
    --r;
    E.resize(n);
    used.resize(n, false);
    g.resize(n), 0;
    for (int i = 1; i < n; i++) {
        int u, v;
        cin >> u >> v;
        --u, --v;
        E[u].push_back({v, i});
        E[v].push_back({u, i});
    }
    grundy(r);
    if (g[r] == 0) cout << 2;
    else {
        fill(used.begin(), used.end(), false);
        dfs(r, 0);
        cout << 1 << '\n' << ans;
    }
    return 0;
}
