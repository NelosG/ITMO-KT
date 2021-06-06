#include <iostream>
#include <queue>
#include <sstream>
#include <vector>

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
    const int n, INF = 1E9;
    int finish;
    int start;
    vector<vector<T>> edges;

    explicit Network(int n) : n(n), start(0), finish(n - 1), edges(n) {}

    void add_edge(int from, int to, int c) {
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

bool bfs_set_depth_tour(Network &net, vector<int> &distance) {
    distance.assign(net.n, -1);
    distance[net.start] = 0;
    queue<int> q;
    q.push(net.start);
    while (!q.empty()) {

        int from = q.front();
        q.pop();
        for (const T &e: net.edges[from])
            if (distance[e.to] == -1 && e.get() > 0) {
                distance[e.to] = distance[from] + 1;
                q.push(e.to);
            }
    }
    return distance[net.finish] != -1;
}

int dfs_find_flow_tour(int from, Network &net, vector<int> &last_block_edge, vector<int> &distance, int flow) {
    if (from == net.finish || flow == 0)
        return flow;

    for (int i = last_block_edge[from]; i < net.edges[from].size(); ++i, ++last_block_edge[from]) {
        T &e = net.edges[from][i];
        if (distance[e.to] != distance[from] + 1) { continue; }
        int delta = dfs_find_flow_tour(e.to, net, last_block_edge, distance, min(e.get(), flow));
        if (delta) {
            e.flow += delta;
            net.get_edge(from, i).flow -= delta;
            return delta;
        }
    }
    return 0;
}

long find_max_flow(Network &net) {
    long max_flow = 0;
    vector<int> distance;
    while (bfs_set_depth_tour(net, distance)) {
        vector<int> last_block_edge(net.n, 0);
        while (int flow = dfs_find_flow_tour(net.start, net, last_block_edge, distance, net.INF))
            max_flow += flow;
    }
    return max_flow;
}

void dfs(int from, Network &net, vector<int> &visited) {
    visited[from] = true;
    for (const T e: net.edges[from])
        if (!visited[e.to] && e.c > e.flow)
            dfs(e.to, net, visited);
}

int outgoing_edges(int i, int j, int length) {
    return length * i + j;
}

int incoming_edges(int i, size_t j, int height, int length) {
    return height * length + outgoing_edges(i, j, length);
}

int main() {
    int l, h;
    cin >> h >> l;
    Network net(h * l * 2);
    vector<vector<char>> table(h, vector<char>(l));
    for (int i = 0; i < h; ++i)
        for (int j = 0; j < l; ++j) {
            cin >> table[i][j];
            switch (table[i][j]) {
                case '-':
                    net.add_edge(incoming_edges(i, j, h, l),
                                 outgoing_edges(i, j, l),
                                 net.INF);
                    break;
                case '.':
                    net.add_edge(incoming_edges(i, j, h, l),
                                 outgoing_edges(i, j, l),
                                 1);
                    break;
                case 'A':
                    net.start = outgoing_edges(i, j, l);
                    break;
                case 'B':
                    net.finish = incoming_edges(i, j, h, l);
            }
        }

    for (int i = 0; i < h; ++i)
        for (int j = 0; j < l; ++j) {
            if (table[i][j] == '#') continue;

            int from = outgoing_edges(i, j, l);
            int to = incoming_edges(i, j, h, l);

            if (i + 1 < h && table[i + 1][j]) {
                net.add_edge(outgoing_edges(i + 1, j, l), to, net.INF);
                net.add_edge(from, incoming_edges(i + 1, j, h, l), net.INF);
            }

            if (j + 1 < l && table[i][j + 1]) {
                net.add_edge(outgoing_edges(i, j + 1, l), to, net.INF);
                net.add_edge(from, incoming_edges(i, j + 1, h, l), net.INF);
            }
        }

    vector<int> Min;
    long min_cut_value = find_max_flow(net);

    vector<int> used(net.n, false);
    dfs(net.start, net, used);
    for (int v = 0; v < net.n; ++v) {
        if (!used[v]) continue;
        for (auto e: net.edges[v])
            if (!used[e.to] && e.flow == 1) {
                Min.push_back(e.to);
                break;
            }
    }

    if (min_cut_value >= net.INF || min_cut_value < 0) {
        cout << -1;
        return 0;
    }

    stringstream ss;
    for (int v: Min)
        table[v / l][v % l] = '+';

    ss << min_cut_value << '\n';
    for (size_t i = 0; i < h; ++i) {
        for (size_t j = 0; j < l; ++j)
            ss << table[i][j];

        ss << '\n';
    }
    cout << ss.str();
}
