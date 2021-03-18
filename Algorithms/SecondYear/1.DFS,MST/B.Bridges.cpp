#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <algorithm>

using namespace std;

void dfs(int v, int e, int &time, vector<bool> &visited,
         vector<int> &in, vector<int> &up,
         map<int, set<pair<int, int>>> &mp, vector<int> &ans) {
    visited[v] = true;
    up[v] = in[v] = time++;
    auto temp = mp.find(v);
    if (temp != mp.end()) {
        for (auto &ui : temp->second) {
            int u = ui.first;
            if(ui.second == e) continue;
            if (!visited[u]) {
                dfs(u, ui.second, time, visited, in, up, mp, ans);
                up[v] = min(up[v], up[u]);
            } else {
                up[v] = min(up[v], in[u]);
            }
        }
        if (up[v] >= in[v] && e > 0) {
            ans.push_back(e);
        }
    }
}

int main() {
    int n, m;
    cin >> n >> m;
    map<int, set<pair<int, int>>> mp;


    for (int i = 1, from, to; i <= m; i++) {
        cin >> from >> to;
        mp[from - 1].insert({to - 1, i});
        mp[to - 1].insert({from - 1, i});
    }

    int time = 0;
    vector<bool> visited;
    vector<int> enter;
    vector<int> ret;
    vector<int> ans;

    visited.resize(n);
    enter.resize(n);
    ret.resize(n);

    for (int i = 0; i < n; i++) {
        visited.push_back(false);
        enter.push_back(0);
        ret.push_back(0);
    }


    for (int i = 0; i < n; i++) {
        if (!visited[i])
            dfs(i, -1, time, visited, enter, ret, mp, ans);
    }

    cout << ans.size() << "\n";

    sort(ans.begin(), ans.end());
    for (int an : ans) {
        cout << an << " ";
    }

    return 0;
}
