#include <vector>
#include <iostream>
#include <algorithm>
#include <set>
#include <list>
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

    list<int> l;

    for (int i = 1; i <= n; i++) {
        l.push_back(i);
    }

    for (int k = 0; k < n * (n - 1); k++) {
        auto it_i = l.begin();
        auto l_beg = it_i++;

        if (!E[*l_beg][*it_i]) {
            int counter = 2;
            auto it_1 = it_i;
            it_i++;
            auto it_i1 = it_i;
            it_i1++;
            while (!E[*l_beg][*it_i] || !E[*it_1][*it_i1]) {
                counter++;
                ++it_i;
                ++it_i1;
            }

            l_beg++;
            for (int i = 1; i <= counter / 2; i++) {
                swap(*l_beg,*it_i);
                l_beg++;
                it_i--;
            }
        }
        l.push_back((*l.begin()));
        l.pop_front();
    }

    for (auto i : l) {
        cout << i << " ";
    }
}
