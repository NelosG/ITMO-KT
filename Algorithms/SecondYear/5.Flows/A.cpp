#include <algorithm>
#include <iostream>
#include <queue>
#include <vector>

using namespace std;

class T {
public:
    int get() const {
        return c - flow;
    }
    T(int to, int c, int index) : flow(0), to(to), c(c), ind(index) {}

    int flow;
    int to, c, ind;
};

int n, m;

void add_edge(int v, int u, int c, vector<pair<int, int>> &O, vector<vector<T>> &G) {
    O.emplace_back(v, G[v].size());
    G[v].emplace_back(u, c, G[u].size());
    G[u].emplace_back(v, 0, G[v].size() - 1);
    G[u].emplace_back(v, c, G[v].size());
    G[v].emplace_back(u, 0, G[u].size() - 1);
}


pair<bool, vector<int>> bfs(int flow, vector<vector<T>> &G) {
    vector<int> depth(n, 1e9);
    depth[0] = 0;
    queue<int> q;
    q.push(0);
    while (!q.empty()) {
        int v = q.front();
        q.pop();
        for (auto &e : G[v]) {
            if (depth[e.to] == 1e9 && e.get() >= flow) {
                depth[e.to] = depth[v] + 1;
                q.push(e.to);
            }
        }
    }
    return {depth[n - 1] != 1e9, move(depth)};
}

int dfs(int v, vector<int> &vec, vector<int> &depth, int flow, vector<vector<T>> &G) {
    if (v == (n - 1) || flow == 0) {
        return flow;
    }
    for (int i = vec[v]; i < G[v].size(); i++) {
        T &e = G[v][i];
        if (depth[e.to] != depth[v] + 1) {
            continue;
        }
        int flow_get = dfs(e.to, vec, depth, min(e.get(), flow), G);
        if (flow_get) {
            e.flow += flow_get;
            G[G[v][i].to][G[v][i].ind].flow -= flow_get;
            return flow_get;
        }
        vec[v]++;
    }
    return 0;
}

long long dinic(vector<vector<T>> &G) {
    int max_cap = 1e9;
    long long answer = 0;
    while (max_cap) {
        long long result = 0;
        auto [gg, level] = bfs(max_cap, G);
        while (gg) {
            vector<int> vec(n, 0);
            long long flow;
            flow = dfs(0, vec, level, 1e9, G);
            while (flow) {
                result += flow;
                flow = dfs(0, vec, level, 1e9, G);
            }
            auto pr = bfs(max_cap, G);
            gg = pr.first;
            level = pr.second;
        }
        answer += result;
        max_cap >>= 1;
    }
    return answer;
}

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    cin >> n >> m;

    vector<vector<T>> G(n);
    vector<pair<int, int>> O;

    for (int i = 0, v, u, c; i < m; i++) {
        cin >> v >> u >> c;
        add_edge(v - 1, u - 1, c, O, G);
    }

    cout << fixed << dinic(G) << endl;

    for (auto num : O) {
        cout << (G[num.first][num.second].flow == 0 ? G[num.first][num.second + 1].flow : G[num.first][num.second].flow) << endl;
    }
    return 0;
}
