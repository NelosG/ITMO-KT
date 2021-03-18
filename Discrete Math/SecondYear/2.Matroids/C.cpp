#include <fstream>
#include <vector>
#include <algorithm>

using namespace std;

#define Debug
#ifdef Debug

#include <iostream>

#endif

vector<vector<int>> G;
vector<int> p, answer;
vector<bool> visited;

bool dfs(int const& v) {
    if (visited[v]) return false;
    visited[v] = true;
    for (auto& to : G[v]) {
        if (p[to] == -1 || dfs(p[to])) {
            p[to] = v;
            answer[v] = to + 1;
            return true;
        }
    }
    return false;
}

int main() {
#ifndef Debug
    ifstream cin("matching.in");
    ofstream cout("matching.out");
#endif

    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    vector<pair<int, int>> weight;
    int n;
    cin >> n;
    G.resize(n);
    p.resize(n, -1);
    answer.resize(n, 0);

    for (int i = 0, w; i < n; i++) {
        cin >> w;
        weight.emplace_back(w, i);
    }
    sort(weight.rbegin(), weight.rend(),
         [](auto const& a, auto const& b) {return a.first < b.first;});

    for (int i = 0, col; i < n; i++) {
        cin >> col;
        for (int j = 0, a; j < col; j++) {
            cin >> a;
            G[i].push_back(a - 1);
        }
    }
    for (int i = 0; i < n; i++) {
        visited.assign(n, false);
        dfs(weight[i].second);
    }

    for (auto& to : answer) {
        cout << to << " ";
    }
}
