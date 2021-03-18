#include <iostream>
#include <vector>
#include <set>

using namespace std;

int main() {
    int n, m;
    cin >> n >> m;
    vector<vector<pair<int, int> > > arr(n);
    for (int i = 0; i < m; ++i) {
        int a, b, c;
        cin >> a >> b >> c;
        a--, b--;
        arr[a].emplace_back(b, c);
        arr[b].emplace_back(a, c);
    }
    vector<int> d(n, 1e9);
    d[0] = 0;
    set<pair<int, int> > set;
    set.insert({0, 0});
    while (!set.empty()) {
        int v = set.begin()->second;
        set.erase(set.begin());
        for (int j = 0; j < arr[v].size(); ++j) {
            if (d[v] + arr[v][j].second < d[arr[v][j].first]) {
                set.erase({d[arr[v][j].first], arr[v][j].first});
                d[arr[v][j].first] = d[v] + arr[v][j].second;
                set.insert({d[arr[v][j].first], arr[v][j].first});
            }
        }
    }
    for (auto i : d) {
        cout << i << " ";
    }
}
