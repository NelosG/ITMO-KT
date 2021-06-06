#include <algorithm>
#include <iostream>
#include <list>
#include <queue>
#include <vector>

using Long = long long;

using namespace std;

const Long INF = INT64_MAX / 1000;

class Tube {
public:
    Long get_free() {
        return c - flow;
    }
    Tube(int to, Long c) : flow(0), to(to), c(c), index(0) {}

    Tube *reverse;
    Long flow, c;
    int to, index;
};

int n;
int start, finish;
vector<list<Tube>> g;

void add_edge(int v, int u, Long c = 1) {
    if (max(v, u) >= g.size()) {
        g.resize(max(v, u) + 1);
    }
    g[v].emplace_back(u, c);
    g[u].emplace_back(v, 0);
    g[v].back().reverse = &g[u].back();
    g[u].back().reverse = &g[v].back();
}

vector<Long> level;

bool bfs(Long flow) {
    level.assign(g.size(), INF);
    level[start] = 0;
    queue<int> q;
    q.push(start);
    while (!q.empty()) {
        int v = q.front();
        q.pop();
        for (auto &e : g[v]) {
            if (level[e.to] == INF && e.get_free() >= flow) {
                level[e.to] = level[v] + 1;
                q.push(e.to);
            }
        }
    }
    return level[finish] != INF;
}

Long dfs(int v, vector<list<Tube>::iterator> &block, vector<Long> &level, Long flow = INF) {
    if (v == finish) return flow;
    if (flow == 0) return 0;
    for (auto i = block[v]; i != g[v].end(); i++) {
        if (level[i->to] != level[v] + 1) continue;
        Long flow_get = dfs(i->to, block, level, min(i->get_free(), flow));
        if (flow_get) {
            i->flow += flow_get;
            i->reverse->flow -= flow_get;
            return flow_get;
        }
        block[v]++;
    }
    return 0;
}

Long dinic() {
    Long max_cap = INF;
    Long answer = 0;
    while (max_cap) {
        Long result = 0;
        bool gg = bfs(max_cap);
        while (gg) {
            vector<list<Tube>::iterator> block(g.size());
            for (int i = 0; i < g.size(); i++) block[i] = g[i].begin();
            Long flow;
            flow = dfs(start, block, level, INF);
            while (flow) {
                result += flow;
                flow = dfs(start, block, level, INF);
            }
            gg = bfs(max_cap);
        }
        answer += result;
        max_cap >>= 1;
    }
    return answer;
}

vector<vector<char>> graph;
int t;


int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);
    cin >> t;
    n = t;
    g.resize(n + 2);
    graph.resize(n, vector<char>(n));
    vector<int> used_at_this_round(n, 0), used(n, 0);
    start = t;
    finish = t + 1;
    for (int i = 0; i < t; i++) {
        for (int j = 0; j < t; j++) {
            cin >> graph[i][j];
            switch (graph[i][j]) {
                case '.':
                    if (i < j) {
                        add_edge(i, j, 3);
                        used[i] += 3;
                    }
                    break;
                case 'W':
                    used_at_this_round[i] -= 3;
                    break;
                case 'w':
                    used_at_this_round[i] -= 2;
                    break;
                case 'l':
                    used_at_this_round[i] -= 1;
                default:
                    break;
            }
        }
    }
    int temp;
    for (int i = 0; i < t; i++) {
        cin >> temp;
        used_at_this_round[i] += temp;
        add_edge(i, t + 1, used_at_this_round[i]);
        add_edge(t, i, used[i]);
    }
    dinic();
    for (int i = 0; i < g.size() - 2; i++) {
        for (auto e : g[i]) {
            if (graph[i][e.to] == '.' && e.to != finish && e.to != start && i < e.to) {
                switch (e.flow) {
                    case 0:
                        graph[i][e.to] = 'W';
                        graph[e.to][i] = 'L';
                        break;
                    case 1:
                        graph[i][e.to] = 'w';
                        graph[e.to][i] = 'l';
                        break;
                    case 2:
                        graph[i][e.to] = 'l';
                        graph[e.to][i] = 'w';
                        break;
                    case 3:
                        graph[i][e.to] = 'L';
                        graph[e.to][i] = 'W';
                        break;
                    default:
                        break;
                }
            }
        }
    }
    for (int i = 0; i < t; i++) {
        for (int j = 0; j < t; j++) {
            cout << graph[i][j];
        }
        cout << endl;
    }
    return 0;
}
