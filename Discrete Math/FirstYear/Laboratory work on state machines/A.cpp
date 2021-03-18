#include <iostream>
using namespace std;

int main() {
    int n, m, k;
    string s;
    freopen("problem1.in", "r", stdin);
    freopen("problem1.out", "w", stdout);
    cin >> s >> n >> m >> k;
    int (*arr)[26];
    arr = new int[n][26];
    int *dop = new int[n];
    for (int i = 0; i < n; i++) {
        dop[i] = 0;
        for (int j = 0; j < 26; j++) {
            arr[i][j] = -1;
        }
    }
    int l;
    for (int i = 0; i < k; i++) {
        cin >> l;
        dop[l - 1] = 1;
    }
    int a, b;
    char c;
    for (int i = 0; i < m; i++) {
        cin >> a >> b >> c;
        arr[a - 1][c - 'a'] = b - 1;
    }
    int len = s.length();
    int prev = 0;
    bool flag = false;
    for (int i = 0; i < len; i++) {
        if(arr[prev][s[i] - 'a'] == -1) {
            flag = true;
            break;
        }
        prev = arr[prev][s[i] - 'a'];
    }
    if (dop[prev] == 0) flag = true;
    if(flag) {
        cout << "Rejects";
    } else {
        cout << "Accepts";
    }
    return 0;
}
