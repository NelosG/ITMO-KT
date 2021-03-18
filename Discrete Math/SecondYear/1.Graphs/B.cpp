#include <vector>
#include <iostream>
#include <algorithm>
#include <deque>
#include <string>



using namespace std;

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int n;
    cin >> n;
    vector<vector<bool>> E(n + 1, vector<bool>(n + 1, false));

    for (int i = 2, ptr; i <= n; i++) {
        string s;
        cin >> s;
        ptr = 1;
        for (char c : s) {
            if (c == '1') {
                E[i][ptr] = true;
                E[ptr][i] = true;
            }
            ptr++;
        }
    }

    deque<int> dq;

    for (int i = 1; i <= n; i++) {
        dq.push_back(i);
    }

    for (int k = 0, i; k < n * (n - 1); k++) {
        if (!E[dq[0]][dq[1]]) {
            i=2;
            while (i < n - 1 && (E[dq[0]][dq[i]] == 0 || E[dq[1]][dq[i + 1]] == 0)) {
                i++;
            }
            if (i == n - 1) {
                i = 2;
                while (!E[dq[0]][dq[i]]) {
                    i++;
                }
            }
            for (int j = 0; j * 2 < i; j++) {
                int temp = dq[j + 1];
                dq[j + 1] = dq[i - j];
                dq[i - j] = temp;
            }
        }
        dq.push_back((*dq.begin()));
        dq.pop_front();
    }

    for (auto i : dq) {
        cout << i << " ";
    }
    return 0;
}
