#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;
 
long long cot = 0;
vector<pair<int, int>> mrg(vector<pair<int, int>> arr1, vector<pair<int, int>> arr2);
 
vector<pair<int, int>> srt (vector<pair<int, int>> arr) {
    if (arr.size() < 2){
        return arr;
    }
    vector<pair<int, int>> arr1;
    vector<pair<int, int>> arr2;
    for(int i = 0; i < arr.size() / 2; i++) {
        arr1.push_back(arr[i]);
    }
    for(int i = arr.size() / 2; i < arr.size(); i++) {
        arr2.push_back(arr[i]);
    }
 
    arr1 = srt(arr1);
    arr2 = srt(arr2);
    return (mrg(arr1, arr2));
}
 
 
 
vector<pair<int, int>> mrg(vector<pair<int, int>> arr1, vector<pair<int, int>> arr2) {
    int n = arr1.size();
    int m = arr2.size();
    vector<pair<int, int>> res;
    for(int i = 0; i < n + m; i++) {
        res.push_back({0, 0});
    }
    int i = 0;
    int j = 0;
    int s = 0;
    while (i < n || j < m) {
        if (i < n  && j < m) {
            if (arr1[i].second <= arr2[j].second) {
                res[s++] = arr1[i++];
            } else {
                res[s++] = arr2[j++];
                cot += n - i;
            }
        } else if (i < n) {
            res[s++] = arr1[i++];
        } else if (j < m) {
            res[s++] = arr2[j++];
        }
    }
    return res;
}
 
int main() {
    freopen("john.in", "r", stdin);
    freopen("john.out", "w", stdout);
    ios::sync_with_stdio(false);
    vector<pair<int, int>> arr;
    int n, k, b;
    cin >> n;
    for (int i = 0; i < n; i++) {
        cin >> k >> b;
        arr.push_back({k, b});
    }
    sort(arr.begin(), arr.end());
    srt(arr);
    cout << cot;
//    for(auto & i : arr) {
//        cout << i.first << " " << i.second << "\n";
//    }
    return 0;
}