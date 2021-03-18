#include <vector>
#include <iostream>
#include <algorithm>
#include <numeric>
#include <random>

using namespace std;
int n;
vector<vector<int>> E;

bool comp(int i, int j ){
    return (E[i][j]);
}


int main() {


    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    cin >> n;
    E.resize(n + 1, vector<int>(n + 1, 0));
    for (int i = 2; i <= n; i++) {
        string s;
        cin >> s;
        int ptr = 1;
        for (char c : s) {
            if (c == '1') {
                E[i][ptr] = 1;
            } else {
                E[ptr][i] = 1;
            }
            ptr++;
        }
    }
    vector<int> v(n);
    iota(v.begin(), v.end(), 1);
    random_device rd;
    mt19937 mersenne(rd());

    while(true) {
        shuffle(v.begin(), v.end(), mersenne);
        stable_sort(v.begin(), v.end(), comp);
        bool flag = true;
        for (int i = 0 ; i < v.size()-1; i++){
            if (!E[v[i]][v[i+1]]) {
                flag = false;
                break;
            }
        }
        if (flag && E[v[n-1]][v[0]]) {
            for (auto i : v){
                cout << i << " ";
            }
            return 0;
        }

    }
}
