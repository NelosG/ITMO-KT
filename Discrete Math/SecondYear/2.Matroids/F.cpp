#include <fstream>
#include <vector>
#include <queue>
#include <unordered_set>

//#define Debug
#ifdef Debug

#include <iostream>

#endif


using namespace std;

struct E {
    int u;
    int v;
    int color;
    int index;
};


int n, m;
vector<E> G;


size_t get(size_t cur, vector<size_t> &dsu) {
    if (dsu[cur] == cur)
        return cur;
    return dsu[cur] = get(dsu[cur], dsu);
}


void fun(unordered_set<int> &set) {
    vector<vector<int>> g_sec(m);
    unordered_set<int> colors;
    vector<size_t> vec(n);
    vector<size_t> r(n);


    for (auto &i : G) {

        if (set.count(i.index)) {

            colors.clear();
            for (size_t j = 0; j < vec.size(); j++)
                vec[j] = j;
            r.assign(r.size(), 0);


            for (auto &j : G) {
                if (i.index != j.index && set.count(j.index)) {
                    colors.insert(j.color);
                    size_t a = get(j.u, vec);
                    size_t b = get(j.v, vec);
                    if (a != b) {
                        if (r[a] < r[b])
                            swap(a, b);
                        vec[b] = a;
                        if (r[a] == r[b])
                            r[a]++;
                    }
                }
            }


            for (auto &j : G) {
                if (!set.count(j.index)) {
                    if (get(j.u, vec) != get(j.v, vec))
                        g_sec[i.index].push_back(j.index);
                    if (!colors.count(j.color))
                        g_sec[j.index].push_back(i.index);
                }
            }

        }
    }

    colors.clear();
    for (size_t i = 0; i < vec.size(); i++)
        vec[i] = i;
    r.assign(r.size(), 0);


    for (auto &i : G) {
        if (set.count(i.index)) {

            colors.insert(i.color);
            size_t a = get(i.u, vec);
            size_t b = get(i.v, vec);
            if (a != b) {
                if (r[a] < r[b])
                    swap(a, b);
                vec[b] = a;
                if (r[a] == r[b])
                    ++r[a];
            }

        }
    }


    vector<int> rg(m, 88888888);
    queue<int> Queue;
    unordered_set<int> y;


    for (auto &i : G) {
        if (!set.count(i.index)) {

            if (!colors.count(i.color)) {
                y.insert(i.index);
                if (rg[i.index] == 0) {
                    set.insert(i.index);

                    vec.clear();
                    r.clear();
                    g_sec.clear();
                    rg.clear();
                    y.clear();
                    colors.clear();
                    while (!Queue.empty()) Queue.pop();

                    fun(set);
                    return;
                }
            }

            if (get(i.u, vec) != get(i.v, vec)) {
                rg[i.index] = 0;
                Queue.push(i.index);
            }

        }
    }

    vector<int> inv(m, -1);


    while (!Queue.empty()) {
        auto v = Queue.front();
        Queue.pop();

        for (auto i : g_sec[v]) {

            if (rg[v] + 1 < rg[i]) {
                inv[i] = v;
                Queue.push(i);
                rg[i] = rg[v] + 1;
            }

        }
    }


    int mx = 88888888;
    int ind = -1;


    for (auto& i : y) {
        if (rg[i] < mx) {
            ind = i;
            mx = rg[i];
        }
    }


    if (ind >= 0) {

        while (ind != -1) {
            if (set.count(ind)) {
                set.erase(ind);
            } else {
                set.insert(ind);
            }
            ind = inv[ind];
        }


        vec.clear();
        r.clear();
        g_sec.clear();
        rg.clear();
        y.clear();
        colors.clear();
        while (!Queue.empty()) Queue.pop();

        fun(set);

    }
}


int main() {
#ifndef Debug
    ifstream cin("rainbow.in");
    ofstream cout("rainbow.out");
#endif

    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);


    unordered_set<int> set;

    cin >> n >> m;
    G.resize(m);


    for (int i = 0, a, b, c; i < m; i++) {
        cin >> a >> b >> c;
        a--;
        b--;
        G[i] = {a, b, c, i};
    }
    fun(set);


    cout << set.size() << "\n";

    for (auto& i : set)
        cout << i + 1 << " ";
    return 0;
}
