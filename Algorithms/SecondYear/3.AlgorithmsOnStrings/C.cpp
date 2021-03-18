#include <iostream>
#include <string>
#include <vector>
#include <algorithm>

using namespace std;


int main() {

    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    string s;
    cin >> s;
    vector<int> zf(s.length() + 1, 0);
    int left = 0, right = 0;
    for (int i = 1; i < s.length(); i++) {
        zf[i] = max(0, min(right - i, zf[i - left]));
        while (i + zf[i] < s.length() && s[zf[i]] == s[i + zf[i]])
            zf[i]++;
        if (i + zf[i] > right) {
            left = i;
            right = i + zf[i];
        }
    }
    for(int i = 1; i < s.length(); i++) {
        cout << zf[i] << " ";
    }
}
