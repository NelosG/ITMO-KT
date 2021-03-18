#include <iostream>
#include <vector>
 
using namespace std;
 
bool del(int n) {
    if (n == 0) return true;
    int kol = 0;
    for (int i = 1; i * i <= n; i++) {
        if (i * i == n) kol++;
        else if (n % i == 0) kol = kol + 2;
    }
    return kol % 3 == 0;
}
 
void swapp(int *a, int *b) {
    int temp = *a;
    *a = *b;
    *b = temp;
}
 
void rev(vector<int> *a, int l, int r) {
    while (r - l > 0) {
        swapp(&(*a)[l], &(*a)[r]);
        l++;
        r--;
    }
}
 
void nextPermutation(vector<int> *a) {
    int n = (*a).size();
    for (int i = n - 2; i >= 0; i--) {
        if ((*a)[i] < (*a)[i + 1]) {
            int min = i + 1;
            for (int j = i + 1; j < n; j++) {
                if (((*a)[j] < (*a)[min]) && ((*a)[j] > (*a)[i])) {
                    min = j;
                }
            }
            swapp(&(*a)[i], &(*a)[min]);
            rev(&(*a), i + 1, n - 1);
            return;
        }
    }
    a->clear();
}
 
int main() {
    freopen("beautiful.in", "r", stdin);
    freopen("beautiful.out", "w", stdout);
    int n, r;
    cin >> n >> r;
    vector<int> a;
    for (int i = 1; i <= n; i++) {
        a.push_back(i);
    }
    int res = 0;
    for (int i = 0; i < 3628800; i++) {
        int temp = 0;
        for (int j = 0; j < n - 1; j++) {
            temp += a[j] * a[j + 1];
        }
        if (del(temp % r)) res++;
//        for(int j = 0; j < n; j++) {
//            cout << a[j] << " ";
//        }
//        cout << "\n";
        nextPermutation(&a);
        if (a.size() == 0) break;
 
    }
    cout << res;
}