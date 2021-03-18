#include <algorithm>
#include <cmath>
#include <iostream>
#include <vector>

using namespace std;


struct ufo {
    ufo(int time, int x, int y) : time(time), x(x), y(y) {}
    int time, x, y;
};


double time_to_travel(ufo a, ufo b, int v) {
    return sqrt((a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y)) * 60 / v;
}

bool dfs(int u, vector<vector<int>> &g, vector<bool> &used, vector<int> &matching, vector<bool> &ans) {
    if (used[u]) return false;
    used[u] = true;
    for (int i : g[u]) {
        if (matching[i] == -1 || dfs(matching[i], g, used, matching, ans)) {
            matching[i] = u;
            ans[u] = true;
            return true;
        }
    }
    return false;
}



int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int v, n;
    cin >> n >> v;
    vector<vector<int>> g(n);
    vector<int> matching(n, -1);
    vector<bool> ans(n, false);
    vector<bool> used(n, false);
    vector<ufo> ufos;

    for (int i = 0, hours, minutes, x, y; i < n; i++) {
        cin >> hours; cin.ignore(1) >>  minutes >> x >> y;
        ufos.emplace_back(minutes + 60 * hours, x, y);
    }

    sort(ufos.begin(), ufos.end(), [](ufo a, ufo b) { return a.time < b.time; });
    for (int i = 0; i < n; i++)
        for (int j = i + 1; j < n; j++)
            if (time_to_travel(ufos[i], ufos[j], v) <= ufos[j].time - ufos[i].time)
                g[i].push_back(j);


    for (int i = 0; i < n; i++) {
        used.assign(n, false);
        dfs(i, g, used, matching, ans);
    }
    int counter = 0;
    for (int i = 0; i < n; i++) {
        counter += ans[i];
    }
    cout << n - counter << endl;
}
