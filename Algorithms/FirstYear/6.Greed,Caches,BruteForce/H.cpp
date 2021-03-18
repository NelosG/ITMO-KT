#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;
 
bool check(vector<int> * a) {
    for(int i = 1; i < (*a).size(); i++) {
        if((*a)[i - 1] == 1 && (*a)[i - 1] == (*a)[i]) return false;
    }
    return true;
}
 
bool next(vector<int> * a) {
    int i = 0;
    bool flag = false;
    for(int k = 0; k < (*a).size(); k++) {
        if((*a)[k] == 0) {
            flag = true;
            break;
        }
    }
    if(flag) {
        while((*a)[i] == 1) {
            (*a)[i] = 0;
            i++;
        }
        (*a)[i] = 1;
        return true;
    }
    return false;
}
int main() {
    freopen("vectors2.in", "r", stdin);
    freopen("vectors2.out", "w", stdout);
    ios::sync_with_stdio(false);
    vector<int> a;
    int n;
    cin >> n;
    for(int i = 0; i < n; i++) {
        a.push_back(0);
    }
    long long res = 0;
    vector<vector<int>> r;
    r.push_back(a);
    while(next(&a)) {
        if(check(&a)){
            r.push_back(a);
        }
    }
    sort(r.begin(), r.end());
    cout << r.size() << '\n';
    for(int i = 0; i < r.size() - 1; i++) {
        for(int j = 0; j < n; j++) {
            cout << r[i][j];
        }
        cout << "\n";
    }
    for(int j = 0; j < n; j++) {
        cout << r[r.size() - 1][j];
    }
    return 0;
}