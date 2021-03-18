#include <iostream>
#include <map>
#include <vector>
#include <algorithm>

using namespace std;
vector<map<int, int>> g;
bool *visited;
int *in;
int *up;
int time = 0;

void dfs(int v, int p) {
    visited[v] = true;
    up[v] = in[v] = time++;
    for (auto &i: g[v]) {    //for (v->u from E)
        int u = i.first;
        if (u == p) continue;
        if (!visited[u]) {
            dfs(u, v);
            up[v] = min(up[v], up[u]);
        } else {
            up[v] = min(up[v], in[u]);
        }
    }
}

int maxColor = 0;

void paint(int v, int color, int parent) {
    visited[v] = true;
    for (auto &i: g[v]) {    //for (v->u from E)
        int u = i.first;
        if (u == parent) continue;
        if (!visited[u]) {
            if (up[u] >= in[v]) {
                i.second = ++maxColor;
                g[u].find(v)->second = maxColor;
                paint(u, maxColor, v);
            } else {
                i.second = color;
                g[u].find(v)->second = color;
                paint(u, color, v);
            }
        } else if (in[u] < in[v]) {
            if (i.second == 0) {
                i.second = color;
                g[u].find(v)->second = color;
            }
        }
    }
}


int main() {
    int n, m;
    cin >> n >> m;
    vector<pair<int, int>> vec;
    for (int i = 0; i < n; i++) {
        g.emplace_back();
    }

    for (int i = 0; i < m; i++) {
        int left, right;
        cin >> left >> right;
        left--;
        right--;
        g[left].insert({right, 0});
        g[right].insert({left, 0});
        vec.emplace_back(left, right);
    }

    visited = (bool *) malloc(sizeof(bool) * n);
    visited = new bool[n];

    in = (int *) malloc(sizeof(int) * n);
    in = new int[n];
    up = (int *) malloc(sizeof(int) * n);
    up = new int[n];
    for (int i = 0; i < n; ++i) {
        visited[i] = false;
        in[i] = up[i] = 0;
    }

    for (int i = 0; i < n; i++) {
        if (!visited[i])
            dfs(i, -1);
    }

    for (int i = 0; i < n; i++) {
        visited[i] = false;
    }

    for (int i = 0; i < n; i++) {
        if (!visited[i]) {
            paint(i, maxColor, -1);
        }
    }
    cout << maxColor << "\n";
    for (auto &i : vec) {
        cout << g[i.first].find(i.second)->second << " ";
    }
}
