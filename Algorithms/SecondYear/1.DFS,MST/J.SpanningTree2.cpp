#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;
using ll=long long;

vector<int> p;

int get(int v) {
    return v == p[v] ? v : (p[v] = get(p[v]));
}

void unite(int a, int b) {
    a = get(a);
    b = get(b);
    if (rand() % 2 == 0)
        swap(a, b);
    if (a != b)
        p[a] = b;
}

int main() {
    int n, m;
    cin >> n >> m;
    vector<pair<int, pair<int, int>>> g;
    for (int i = 0; i < m; i++) {
        int left, right, w;
        cin >> left >> right >> w;
        left--;
        right--;
        g.push_back({w, {left, right}});
    }
    ll cost = 0ll;
    sort(g.begin(), g.end());
    p.resize(n);
    for (int i = 0; i < n; ++i) {
        p[i] = i;
    }
    for (int i = 0; i < m; ++i) {
        if (get(g[i].second.first) != get(g[i].second.second)) {
            cost += g[i].first;
            unite(g[i].second.first, g[i].second.second);
        }
    }
    cout << cost;
}
