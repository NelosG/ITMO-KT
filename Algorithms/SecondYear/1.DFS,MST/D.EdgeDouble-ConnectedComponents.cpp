#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <algorithm>

using namespace std;

map<int, set<pair<int, int>>> mp;
vector<bool> visited;
vector<int> in;
vector<int> up;
int time = 0;

void dfs(int v, int e) {
    visited[v] = true;
    up[v] = in[v] = time++;
    auto temp = mp.find(v);
    if (temp != mp.end()) {
        for (auto &ui : temp->second) {
            int u = ui.first;
            if (ui.second == e) continue;
            if (!visited[u]) {
                dfs(u, ui.second);
                up[v] = min(up[v], up[u]);
            } else {
                up[v] = min(up[v], in[u]);
            }
        }
    }
}

vector<int> colors;
int maxColor = 0;


void paint(int v, int color) {
    colors[v] = color;
    auto temp = mp.find(v);
    if (temp != mp.end()) {
        for (auto &ui : temp->second) {
            int u = ui.first;
            if (colors[u] == 0) {
                if (up[u] > in[v]) {
                    maxColor++;
                    paint(u, maxColor);
                } else {
                    paint(u, color);
                }
            }
        }
    }
}

int main() {
    int n, m;
    cin >> n >> m;


    for (int i = 1, from, to; i <= m; i++) {
        cin >> from >> to;
        mp[from - 1].insert({to - 1, i});
        mp[to - 1].insert({from - 1, i});
    }


    visited.resize(n);
    in.resize(n);
    up.resize(n);

    for (int i = 0; i < n; i++) {
        visited.push_back(false);
        colors.push_back(0);
        in.push_back(0);
        up.push_back(0);
    }


    for (int i = 0; i < n; i++) {
        if (!visited[i])
            dfs(i, -1);
    }

    for (int i = 0; i < n; i++) {
        if (colors[i] == 0) {
            maxColor++;
            paint(i, maxColor);
        }
    }

    cout << maxColor << "\n";
    for (int &i : colors) {
        cout << i << " ";
    }


    return 0;
}
