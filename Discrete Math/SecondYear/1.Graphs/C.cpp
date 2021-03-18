#include <algorithm>
#include <iostream>
#include <numeric>
#include <string>
#include <vector>


using namespace std;
bool compare(int i, int j) {
    cout << 1 << " " << i << " " << j << "\n";
    cout.flush();
    string ans;
    cin >> ans;
    return (ans == "YES");
}

void merge(vector<int> &a, int left, int mid, int right) {
    int it1 = 0;
    int it2 = 0;
    vector<int> result;
    result.resize(right - left, 0);


    while (left + it1 < mid && mid + it2 < right) {
        if (compare(a[left + it1], a[mid + it2])) {
            result[it1 + it2] = a[left + it1];
            it1 += 1;
        } else {
            result[it1 + it2] = a[mid + it2];
            it2 += 1;
        }
    }

    while (left + it1 < mid) {
        result[it1 + it2] = a[left + it1];
        it1 += 1;
    }

    while (mid + it2 < right) {
        result[it1 + it2] = a[mid + it2];
        it2 += 1;
    }

    for (int i = 0; i < it1 + it2; i++)
        a[left + i] = result[i];
}

void mergeSortRecursive(vector<int> &a, int left, int right) {
    if (left + 1 >= right)
        return;
    int mid = (left + right) / 2;
    mergeSortRecursive(a, left, mid);
    mergeSortRecursive(a, mid, right);
    merge(a, left, mid, right);
}
int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);
    int n;
    cin >> n;

    vector<int> vec(n);
    iota(vec.begin(), vec.end(), 1);
    mergeSortRecursive(vec, 0, n);
    cout << 0 << " ";
    for (auto i : vec) {
        cout << i << " ";
    }
}
