#include <iostream>
#include <vector>

using namespace std;



void dfs(int v, vector<vector<int>> const& g, vector<int> & parent) {
    for (size_t i = 0; i < g[v].size(); ++i) {
        if (g[v][i] != parent[v]) {
            parent[g[v][i]] = v;
            dfs(g[v][i], g, parent);
        }
    }
}


int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int n;
    cin >> n;
    vector<vector<int>> g(n, vector<int>());
    vector<int> parent(n, 0), degree(n, 0);

    for (int i = 0, u, v; i < n - 1; ++i) {
        cin >> u >> v;
        g[--u].push_back(--v);
        g[v].push_back(u);
    }


    parent[n - 1] = -1;
    dfs(n - 1, g, parent);

    int ptr = -1;
    for (int i = 0; i < n; ++i) {
        degree[i] = (int) g[i].size();
        if (degree[i] == 1 && ptr == -1)
            ptr = i;
    }

    vector<int> result;
    int leaf = ptr;
    for (int i = 0; i < n - 2; ++i) {
        int next = parent[leaf];
        result.push_back(next);
        --degree[next];
        if (degree[next] == 1 && next < ptr)
            leaf = next;
        else {
            ++ptr;
            while (ptr < n && degree[ptr] != 1)
                ++ptr;
            leaf = ptr;
        }
    }

    for (auto i : result) {
        cout << (i + 1) << " ";
    }
}
