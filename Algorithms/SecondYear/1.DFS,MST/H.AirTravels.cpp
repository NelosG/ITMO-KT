#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;
int n;
vector<vector<int>> arr;
vector<bool> visited;
vector<int> component;
vector<int> tpsort;
int capacity = 0;
int color = 1;

void dfs(int v) {
    visited[v] = true;
    for (int i = 0; i < n; i++) {
        if(i == v) continue;
        if(capacity >= arr[v][i]) {
            if (!visited[i]) {
                dfs(i);
            }
        }
    }
    tpsort.push_back(v);
}

void dfsT(int v) {
    component[v] = color;
    for (int i = 0; i < n; i++) {
        if(i == v) continue;
        if(capacity >= arr[i][v]) {
            if (component[i] == 0) {
                dfsT(i);
            }
        }
    }
}
bool check() {
    for (int i = 0; i < n; ++i) {
        visited[i] = false;
        component[i] = 0;
    }
    color = 1;

    for (int i = 0; i < n; i++) {
        if (!visited[i]) {
            dfs(i);
        }
    }

    reverse(tpsort.begin(), tpsort.end());
    for (int i = 0; i < n; i++) {
        if (component[tpsort[i]] == 0) {
            dfsT(tpsort[i]);
            color++;
        }
    }

    return color == 2;
}

int main() {
    freopen("avia.in", "r", stdin);
    freopen("avia.out", "w", stdout);
    cin >> n;
    arr.resize(n,vector<int>(n));
    visited.resize(n,false);
    component.resize(n, 0);
    int maxCap = -1;
    for(int i = 0, tmp; i < n; i++) {
        for(int j = 0; j < n; j++) {
            cin >> tmp;
            arr[i][j] = tmp;
            if(tmp > maxCap) maxCap = tmp;
        }
    }
    int minCap = -1; maxCap++;
    while(maxCap - minCap > 1) {
        capacity = minCap + (maxCap - minCap)/2 ;

        if(check()) {
            maxCap = capacity;
        } else {
            minCap = capacity;
        }
    }
    cout << maxCap;
}
