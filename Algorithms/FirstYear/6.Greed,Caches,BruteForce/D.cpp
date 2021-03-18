#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;
 
 
int main() {
    freopen("sequence.in", "r", stdin);
    freopen("sequence.out", "w", stdout);
    ios::sync_with_stdio(false);
    int n;
    cin >> n;
    vector<pair<int, int>> arr;
    int x;
    pair<int, int> temp;
    for (int i = 0; i < n; i++) {
        cin >> x;
        temp.first = x;
        temp.second = i;
        arr.push_back(temp);
    }
    int sumA = 0;
    int sumB = 0;
    vector<pair<int, int>> B;
    sort(arr.begin(), arr.end());
    for (int i = arr.size() - 1; i >= 0; i--) {
        if (sumA < sumB) sumA += arr[i].first;
        else {
            B.push_back(arr[i]);
            sumB += arr[i].first;
        }
    }
    if (sumB != sumA) cout << -1;
    else {
        cout << B.size() << '\n';
        for (int i = 0; i < B.size(); i++) {
            cout << B[i].second + 1 << " ";
        }
    }
    return 0;
}