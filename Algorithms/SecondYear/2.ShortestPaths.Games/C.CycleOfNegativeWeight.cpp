#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;
struct edge {
    int a, b, w;
};

int main() {
    int n;
    cin >> n;
    vector<edge> E;
    vector<int> d(n + 1, 0);
    vector<int> p(n + 1, -1);
    for (int i = 1; i <= n; i++) {
        for (int j = 1; j <= n; j++) {
            int a;
            cin >> a;
            if (a == 100000) continue;
            E.push_back({i, j, a});
        }
    }
    for (int i = 0; i < n; i++) {
        for (auto e : E) {
            if (d[e.b] > d[e.a] + e.w) {
                d[e.b] = d[e.a] + e.w;
                p[e.b] = e.a;
            }
        }
    }


    vector<int> ans;

    for (auto e : E)
        if (d[e.b] > d[e.a] + e.w) {
            int v = e.b, u;
            for (int i = 0; i < n; i++) {
                v = p[v];
            }

            u = v;
            v = p[v];
            ans.push_back(u);
            while (u != v) {
                ans.push_back(v);
                v = p[v];
            }
            break;
        }

    if (ans.empty()) cout << "NO";
    else {
        cout << "YES\n" << ans.size() << "\n";
        for (auto i = ans.rbegin(); i != ans.rend(); ++i) {
            cout << *i << " ";
        }
    }
}
