#include <iostream>
#include <string>
#include <vector>

using namespace std;


int main() {

    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);
    string s;
    cin >> s;
    int n = s.length();
    vector<int> prefix(n + 1, 0);
    prefix[0] = -1;
    for (int i = 1, count; i <= n; i++) {
        count = prefix[i - 1];
        while (count >= 0) {
            if (s[count] == s[i - 1]) {
                prefix[i] = count + 1;
                break;
            } else {
                count = prefix[count];
            }
        }
    }
    int k = (n - prefix[n]);
    cout << (n % k == 0 ? k : n);
}
