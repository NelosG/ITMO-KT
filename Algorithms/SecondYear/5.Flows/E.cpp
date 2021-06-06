#include <iostream>
#include <algorithm>
#include <vector>
#include <queue>
#include <list>

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

int n, m, computers;
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

int index(int day, int max_v, int v) {
    return day * max_v + v;
}

vector <pair <int, int> > graph;



int solver() {
    int days = 0;
    int trans_comp = 0;
    int finisher = finish;
    for (days = 0; trans_comp < computers; days++) {
        for (int i = 0; i < n; i++) {
            add_edge(index(days, n, i), index(days + 1, n, i), INF);
        }
        for (int i = 0; i < m; i++) {
            add_edge(index(days, n, graph[i].first), index(days + 1, n, graph[i].second));
            add_edge(index(days, n, graph[i].second), index(days + 1, n, graph[i].first));
        }
        finish = index(days + 1, n, finisher);
        trans_comp += dinic();
    }
    return days;
}



int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    cin >> n >> m >> computers;
    cin >> start >> finish;
    start--, finish--;
    graph.resize(m);
    for (int i = 0; i < m; i++) {
        cin >> graph[i].first >> graph[i].second;
        graph[i].first--, graph[i].second--;
    }
    int answer = solver();

    cout << answer << endl;
    vector <int> temp(computers, start);
    for (int i = 0; i < answer; i++) {
        list <pair <int, int> > trans;
        for (int j = 0; j < computers; j++) {
            for (auto &e : g[temp[j]]) {
                if (e.flow >= 1) {
                    e.flow -= 1;
                    if (temp[j] + n != e.to) {
                        trans.emplace_back(j, e.to % n);
                    }
                    temp[j] = e.to;
                    break;
                }
            }
        }
        cout << trans.size() << "  ";
        for (auto it : trans) {
            cout << it.first + 1 << ' ' << it.second + 1 << "  ";
        }
        cout << endl;
    }

    return 0;
}
