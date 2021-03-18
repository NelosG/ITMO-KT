#include <iostream>
#include <set>
#include <vector>
#include <algorithm>
using namespace std;
vector<set<pair<int,int>>> g;
vector<set<pair<int,int>>> gr;
vector<bool> visited;
vector<int> component;
vector<int> order;
int color = 1;

void dfs(int v) {
    visited[v] = true;
    for (auto& i: g[v]) {
        if (!visited[i.second]) {
            dfs(i.second);
        }
    }
    order.push_back(v);
}

void dfsT(int v) {
    component[v] = color;
    for (auto& i: gr[v]) {
        if (component[i.second] == 0) {
            dfsT(i.second);
        }
    }
}


int main() {
    int n, m;
    cin >> n >> m;
    for (int i = 0; i < n; i++) {
        g.emplace_back();
        gr.emplace_back();
    }

    for (int i = 0; i < m; i++) {
        int left, right;
        cin >> left >> right;
        left--;
        right--;
        g[left].insert({left, right});
        gr[right].insert({right, left});
    }

    visited.resize(n,false);
    component.resize(n, 0);
    for (int i = 0; i < n; ++i) {
        visited[i] = false;
        component[i] = 0;
    }

    for (int i = 0; i < n; i++) {
        if (!visited[i]) {
            dfs(i);
        }
    }


    for (auto i = order.rbegin(); i != order.rend(); i++) {
        if (component[*i] == 0) {
            dfsT(*i);
            color++;
        }
    }

    set<pair<int, int>> answ;
    for (int i = 0; i < n; i++) {
        for (auto& e : g[i]) {
            if (component[e.first] != component[e.second]) {
                int mi = min(component[e.first], component[e.second]);
                int ma = max(component[e.first], component[e.second]);
                answ.insert({mi,ma});
            }
        }
    }
    cout << answ.size();
}
