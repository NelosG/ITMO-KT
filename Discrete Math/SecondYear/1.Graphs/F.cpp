#include <iostream>
#include <set>
#include <vector>
using namespace std;

int main() {
    int n;
    cin >> n;
    vector<int> pruf(n-2, 0);
    vector<int> degree(n, 1);

    for(int i = 0, v; i < n-2; i++) {
        cin >> pruf[i];
        --pruf[i];
    }

    for (int i = 0; i < n - 2; ++i)
        ++degree[pruf[i]];

    set<int> leaves;
    for (int i = 0; i < n; ++i)
        if (degree[i] == 1)
            leaves.insert(i);

    vector<pair<int, int>> result;
    for (int i = 0; i < n - 2; ++i) {
        int leaf = *leaves.begin();
        leaves.erase(leaves.begin());

        int v = pruf[i];
        result.emplace_back(leaf, v);
        if (--degree[v] == 1)
            leaves.insert(v);
    }
    result.emplace_back(*leaves.begin(), *--leaves.end());

    for(auto [u, v] : result){
        cout << u + 1 << ' ' << v + 1 << '\n';
    }
}
