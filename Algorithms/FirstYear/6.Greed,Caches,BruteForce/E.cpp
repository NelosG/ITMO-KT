#include <iostream>
#include <vector>
#include <algorithm>
 
using namespace std;
 
 
int main() {
    freopen("apples.in", "r", stdin);
    freopen("apples.out", "w", stdout);
    ios::sync_with_stdio(false);
    vector<vector<pair<pair<int, int>, int>>> ar;
    ar.push_back(vector<pair<pair<int, int>, int>>());
    ar.push_back(vector<pair<pair<int, int>, int>>());
    int n, s, down, up;
    cin >> n >> s;
    for(int i = 1; i <= n; i++) {
        cin >> down >> up;
        if(up >= down) {
            ar[0].push_back({{down, up}, i});
        } else {
            ar[1].push_back({{up, down}, i});
        }
    }
    sort(ar[0].begin(), ar[0].end());
    sort( ar[1].begin(), ar[1].end());
    reverse(ar[1].begin(), ar[1].end());
    bool flag = true;
    vector<int> res;
    for(pair<pair<int, int>, int> i: ar[0]) {
        res.push_back(i.second);
        s -= i.first.first;
        if(s <= 0) {
            flag = false;
            break;
        }
        s+= i.first.second;
    }
    if(flag) {
        for(pair<pair<int, int>, int> i: ar[1]) {
            res.push_back(i.second);
            s -= i.first.second;
            if(s <= 0) {
                flag = false;
                break;
            }
            s+= i.first.first;
        }
    }
    if(flag) {
        for(int i : res) {
            cout << i << " ";
        }
    } else {
        cout << -1;
    }
 
}