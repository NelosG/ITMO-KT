#include <iostream>
#include <vector>
#include <unordered_set>
#include <string>
#include <climits>
#include <algorithm>


using namespace std;

int const x = 5167;

vector<vector<int>> hashes;
vector<string> strings;
vector<int> pw;

string substring(int l)
{
    unordered_set<int> set;

    for (int j = 0; j < hashes[0].size() - l; ++j) {
        set.insert(hashes[0][j + l] - hashes[0][j] * pw[l]);
    }

    int ir = -1, jr;

    for (int i = 0; i < hashes.size(); ++i) {
        unordered_set<int> next_set;
        for (int j = 0; j < hashes[i].size() - l; ++j) {
            auto hash = hashes[i][j + l] - hashes[i][j] * pw[l];
            if (set.count(hash)) {
                next_set.insert(hash);
                if (i == hashes.size() - 1) {
                    ir = i;
                    jr = j;
                    break;
                }
            }
        }
        set = next_set;
    }

    return ir >= 0 ? strings[ir].substr(jr, l)  : string();
}


int main()
{
    size_t k;
    cin >> k;

    hashes.resize(k);
    strings.resize(k);

    int l = -1;
    int r = INT_MAX;
    int N = 0;

    for (int i = 0; i < k; ++i) {
        cin >> strings[i];
        N = max(N, (int) strings[i].size() + 1);
        r = min(r, (int) strings[i].size() + 1);

        hashes[i].resize(strings[i].size() + 1);
        hashes[i][0] = 0;
        for (int j = 1; j < hashes[i].size(); j++) {
            hashes[i][j] = hashes[i][j - 1] * x + strings[i][j - 1];
        }
    }

    ++N;
    pw.resize(N);
    pw[0] = 1;
    for (int i = 1; i < N; ++i) {
        pw[i] = x * pw[i - 1];
    }

    string result;
    string s;
    int mid;
    while (r - l > 1) {
        mid = (l + r) / 2;
        s = substring(mid);
        if (!s.empty() || !mid) {
            result = s;
            l = mid;
        } else r = mid;
    }

    cout << result;
    return 0;
}
