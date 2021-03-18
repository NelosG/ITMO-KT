#include <iostream>
#include <vector>
#include <map>
#include <set>

using namespace std;

bool dfs_l(int v, vector<int> &color, const map<int, set<int>>& mp) {
    color[v] = 1;
    auto temp = mp.find(v);
    if(temp != mp.end()) {
        for (auto &u : temp->second) {
            if (color[u] == 0)
                if (dfs_l(u, color, mp))
                    return true;
            if (color[u] == 1)
                return true;
        }
    }
    color[v] = 2;
    return false;
}

bool cycles(int n, const map<int, set<int>>& mp) {
    vector<int> colors;
    colors.resize(n);
    for (int i = 0; i < n; i++) colors.push_back(0);

    for (auto &i : mp) {
        if (colors[i.first] != 2) {
            if (dfs_l(i.first, colors, mp)) {
                return true;
            }
        }
    }
    return false;
}

void dfs(int u, vector<int> &ans, vector<bool> &visited, const map<int, set<int>>& mp) {
    visited[u] = true;
    auto temp = mp.find(u);
    if(temp != mp.end()) {
        for (auto &v : temp->second)
            if (!visited[v])
                dfs(v, ans, visited, mp);
    }
    ans.push_back(u);
}

void topologicalSort(int n, vector<int> &ans, const map<int, set<int>>& mp) {
    if (cycles(n, mp)) {
        ans.push_back(-2);
        return;
    }
    vector<bool> visited;
    visited.resize(n);
    for (int i = 0; i < n; i++) visited.push_back(false);

    for (int v = 0; v < n; v++)
        if (!visited[v])
            dfs(v, ans, visited, mp);
}


int main() {
    int n, m;
    cin >> n >> m;
    map<int, set<int>> mp;


    for (int i = 0, from, to; i < m; i++) {
        cin >> from >> to;
        mp[from - 1].insert(to - 1);
    }

    vector<int> ans;
    topologicalSort(n, ans, mp);
    for(auto an = ans.rbegin(); an != ans.rend(); i++) {
        cout << an + 1 << " ";
    }



    return 0;
}
