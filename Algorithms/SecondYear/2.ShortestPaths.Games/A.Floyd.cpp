#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;
vector<vector<int>> d;
int n;

void floyd() {
    for(int i = 0; i < n; ++i) {
        for(int u = 0; u < n; ++u) {
            for(int v = 0; v < n; ++v) {
                d[u][v] = min(d[u][v], d[u][i] + d[i][v]);
            }
        }
    }
}
int main() {
    cin >> n;
    d.resize(n, vector<int>(n));
    for(int i = 0; i < n; ++i) {
        for(int j = 0; j < n; ++j) {
            cin >> d[i][j];
        }
    }
    floyd();
    for(int i = 0; i < n; ++i) {
        for(int j = 0; j < n; ++j) {
            cout << d[i][j] << " ";
        }
        cout << '\n';
    }
    return 0;
}
