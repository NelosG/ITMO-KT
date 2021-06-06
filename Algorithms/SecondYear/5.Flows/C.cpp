#include <iostream>
#include <vector>
#include <sstream>
#include <cstdint>
#include <algorithm>
using namespace std;

struct T {
    int to, c, reverse_index;
    int flow = 0;

    T(int to, int c, int reverse_index) {
        this->to = to;
        this->c = c;
        this->reverse_index = reverse_index;

    }

    int get() const { return c - flow; }
};

class Network {
public:
    const int n, m, start, finish, INF = INT32_MAX;
    vector<vector<T>> edges;

    Network(int n, int m, int start, int finish) : n(n), m(m), start(start), finish(finish), edges(n) {}

    void add_edge(int from, int to, int c = 1) {
        int reverse_from_index = edges[to].size();
        int reverse_to_index = edges[from].size();
        edges[from].emplace_back(to, c, reverse_from_index);
        edges[to].emplace_back(from, 0, reverse_to_index);
    }

    T &get_edge(int from, int idx) {
        T e = edges[from][idx];
        return edges[e.to][e.reverse_index];
    }
};

int dfs(int from, Network &net, vector<int> &visited, int flow) {
    visited[from] = true;
    if (from == net.finish || flow == 0)
        return flow;

    for (int i = 0; i < net.edges[from].size(); ++i) {
        T &e = net.edges[from][i];
        if (!visited[e.to] && e.get() > 0) {
            int delta = dfs(e.to, net, visited, min(e.get(), flow));
            if (delta) {
                e.flow += delta;
                net.get_edge(from, i).flow -= delta;
                return delta;
            }
        }
    }
    return 0;
}

void make_path(int from, Network &net, vector<int> &visited, vector<int> &path) {
    path.push_back(from);
    visited[from] = true;
    if (from == net.finish) { return; }
    for (T &e: net.edges[from])
        if (!visited[e.to] && e.flow > 0) {
            e.flow = 0;
            return make_path(e.to, net, visited, path);
        }
}

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int n, m, s, t, from, to;
    cin >> n >> m >> s >> t;
    Network net(n, m, --s, --t);
    vector<int> used;
    for (int i = 0; i < m; ++i) {
        cin >> from >> to;
        net.add_edge(from - 1,to - 1);
    }
    for (int i = 0; i < 2; ++i) {
        used.assign(n, false);
        if (!dfs(net.start, net, used, net.INF)) {
            cout << "NO";
            return 0;
        }
    }
    stringstream ss;
    ss << "YES\n";
    for (int i = 0; i < 2; ++i) {
        used.assign(n, false);
        vector<int> path;
        make_path(net.start, net, used, path);
        for (int v: path) {
            ss << (v + 1)  <<  ' ';
        }
        ss << '\n';
    }
    cout << ss.str() ;
}
