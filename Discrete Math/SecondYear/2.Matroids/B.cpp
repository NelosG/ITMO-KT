#include <fstream>
#include <vector>
#include <algorithm>

typedef long long int ll;

#define Debug
#ifdef Debug

#include <iostream>

#endif

using namespace std;

vector<int> value, parent;


int find_set(int a) {
    if (parent[a] == a) return a;
    parent[a] = find_set(parent[a]);
    return parent[a];
}

int main() {
#ifndef Debug
    ifstream cin("destroy.in");
    ofstream cout("destroy.out");
#endif

    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int n, m, a, b;
    ll s, c, cur_cost = 0;
    cin >> n >> m >> s;
    value.resize(n, 1);
    parent.resize(n);
    for (int i = 0; i < n; i++) {
        parent[i] = i;
    }

    vector<pair<pair<ll, int>,pair<int,int>>> E(m);
    for (int i = 0; i < m; i++) {
        cin >> a >> b >> c;
        E[i] = {{c,i + 1 },{a - 1, b - 1}};
    }
    sort(E.begin(), E.end(),
         [](const auto &a, const auto &b) { return a.first.first > b.first.first; });

    vector<int> unused;
    vector<int> answer;

    for (int i = 0; i < m; i++) {
        if (find_set(E[i].second.first) != find_set(E[i].second.second)) {
            a = find_set(E[i].second.first);
            b = find_set(E[i].second.second);
            if (a != b) {
                if (value[a] > value[b])
                    std::swap(a, b);
                parent[a] = b;
                value[b] += value[a];
            }
        } else {
            unused.push_back(i);
        }
    }

    for(auto i = unused.rbegin(); i != unused.rend(); i++) {
        if (cur_cost + E[*i].first.first <= s) {
            cur_cost += E[*i].first.first;
            answer.push_back(E[*i].first.second);
        }
    }

    sort(answer.begin(), answer.end());
    cout << answer.size() << "\n";
    for (auto ind : answer)
        cout << ind << " ";
}
