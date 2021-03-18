#include <iostream>
#include <vector>
#include <sstream>

using namespace std;
bool dfs(int u, vector<vector<int>> &g, vector<bool> &used, vector<int> &a) {
    used[u] = true;
    for (int v : g[u]) {
        if (a[v] == -1) {
            a[v] = u;
            return true;
        }
        if (!used[a[v]] && dfs(a[v], g, used, a)) {
            a[v] = u;
            return true;
        }
    }
    return false;
}

int main() {
    int n, m;
    cin >> n >> m;
    vector<vector<int>> g(n, vector<int>());
    for (int i = 0,x; i < n; i++)
        while ((cin >> x,x) != 0)
            g[i].push_back(x - 1);

    vector<bool> used(n, false);
    vector<int> matching(m, -1);
    for (int i = 0; i < n; i++){
        used.assign(n, false);
        dfs(i, g, used, matching);
    }

    int res = 0;
    stringstream out;
    for (int i = 0; i < m; ++i)
        if (matching[i] != -1) {
            out << matching[i] + 1 << " " << i + 1 << '\n';
            ++res;
        }
    cout << res << '\n' << out.str();

}
