#include <algorithm>
#include <iostream>
#include <queue>
#include <vector>

using Long = long long;

using namespace std;

const Long INF = INT64_MAX / 1000;

class T {
public:
    Long get() const {
        return c - flow;
    }
    T(int to, Long c, int index) : flow(0), to(to), c(c), index(index) {}

    Long flow, c;
    int to, index;
};

int n, m;


void add_small_edge(int v, int u, Long c, vector<vector<T>> &g) {
    g[v].emplace_back(u, c, g[u].size());
    g[u].emplace_back(v, 0, g[v].size() - 1);
}

void add_edge(int v, int u, Long c,vector<pair<int, int>> & order, vector<vector<T>> &g) {
    order.emplace_back(v, g[v].size());
    add_small_edge(v, u, c, g);
    add_small_edge(u, v, c, g);
}

vector<Long> depth;

bool bfs(Long flow, vector<vector<T>> &g) {
    depth.assign(g.size(), INF);
    depth[0] = 0;
    queue<int> q;
    q.push(0);
    while (!q.empty()) {
        int v = q.front();
        q.pop();
        for (auto &e : g[v]) {
            if (depth[e.to] == INF && e.get() >= flow) {
                depth[e.to] = depth[v] + 1;
                q.push(e.to);
            }
        }
    }
    return depth[n - 1] != INF;
}

Long dfs(int v, vector<int> &block, vector<vector<T>> &g, Long flow = INF) {
    if (v == n - 1) return flow;
    if (flow == 0) return 0;
    for (auto i = block[v]; i < g[v].size(); i++) {
        auto &it = g[v][i];
        if (depth[it.to] != depth[v] + 1) continue;
        Long flow_get = dfs(it.to, block, g, min(it.get(), flow));
        if (flow_get) {
            it.flow += flow_get;
            g[g[v][i].to][g[v][i].index].flow -= flow_get;
            return flow_get;
        }
        block[v]++;
    }
    return 0;
}

Long dinic(vector<vector<T>> &g) {
    Long max = INF;
    Long answer = 0;
    while (max) {
        Long result = 0;
        bool gg = bfs(max, g);
        while (gg) {
            vector<int> bl(g.size(), 0);
            Long flow;
            flow = dfs(0, bl, g);
            while (flow) {
                result += flow;
                flow = dfs(0, bl, g);
            }
            gg = bfs(max, g);
        }
        answer += result;
        max >>= 1;
    }
    return answer;
}

vector<bool> used;

void used_dfs(int v, vector<vector<T>> &g) {
    if (used[v]) return;
    used[v] = true;
    for (auto e : g[v]) {
        if (e.c != e.flow) {
            used_dfs(e.to, g);
        }
    }
}



int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);
    cin >> n >> m;

    vector<vector<T>> g(n);
    vector<pair<int, int>> order;
    Long answer;
    vector<int> answer_list;

    Long c;
    for (int i = 0, v, u; i < m; i++) {
        cin >> v >> u >> c;
        add_edge(--v, --u, c, order, g);
    }
    dinic(g);
    used.resize(n, false);
    used_dfs(0, g);
    answer = 0;
    int index = -1;
    for (auto e : order) {
        index++;
        if (used[e.first] == used[g[e.first][e.second].to]) continue;
        Long flow = g[e.first][e.second].flow;
        if (flow == 0) {
            flow = g[e.first][e.second + 1].flow;
        }
        answer += abs(flow);
        answer_list.emplace_back(index);
    }
    cout << answer_list.size() << ' ' << answer << endl;
    for (int it : answer_list) cout << it + 1 << ' ';
    return 0;
}
