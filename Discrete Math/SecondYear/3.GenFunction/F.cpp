#include <functional>
#include <iostream>
#include <map>
#include <vector>

using namespace std;

int MOD = 1e9 + 7;
using Long = long long;
map<int, int> mem;

int get(int in, const function<int(int)>& f) {
    return mem.count(in) ? mem[in] : (mem[in] = f(in));
}

Long Tree(int m, vector<Long>& trees, vector<int>& c) {
    if (m < 0) return 0;
    if (trees[m] == -1) {
        trees[m] = 0;
        for (int rootWeight : c) {
            trees[m] = (trees[m] +  get(m - rootWeight, [&trees, &c](int sum) {
              Long res = 0;
              for (int leftWeight = 0; leftWeight <= sum; leftWeight++)
                  res = (res +  Tree(leftWeight, trees, c) * Tree(sum - leftWeight, trees, c) % MOD) % MOD;
              return res;
            })) % MOD;
        }
    }
    return trees[m];
}

int main() {
    int k, m;
    cin >> k >> m;
    vector<int> c(k, 0);
    for (int i = 0; i < k; cin >> c[i++]);
    vector<Long> trees(m + 1, -1);
    trees[0] = 1;
    for (int i = 1; i <= m; Tree(i++, trees, c));
    for (int i = 1; i < trees.size(); cout << trees[i++] << ' ');
}
