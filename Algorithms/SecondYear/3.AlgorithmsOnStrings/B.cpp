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
    vector<int> prefix(s.length() + 1, 0);
    prefix[0] = -1;
    for (int i = 1, count; i <= s.length(); i++) {
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
    for (int i = 1; i <= s.length(); i++) {
        cout << prefix[i] << " ";
    }
}
