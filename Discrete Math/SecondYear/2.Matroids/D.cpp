#include <fstream>
#include <map>

#define Debug
#ifdef Debug

#include <iostream>

#endif

using namespace std;

int main() {
#ifndef Debug
    std::ifstream cin("check.in");
    std::ofstream cout("check.out");
#endif

    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int n, m;
    cin >> n >> m;

    int sub = (1 << n);
    map<int, int> map;

    for (int i = 0, count; i < m; i++) {
        cin >> count;
        int hash = 0;
        for (int j = 0, elem; j < count; j++) {
            cin >> elem;
            hash = (hash | (1 << (--elem)));
        }
        map[hash] = count;
    }


    if (!map.count(0)) {
        cout << "NO";
        return 0;
    }

    for (auto& a : map)
        for (int b = 0; b < sub; b++)
            if (b == (b & a.first) && !map.count(b)) {
                cout << "NO";
                return 0;
            }

    for (auto& a : map) {
        for (auto& b : map) {
            if (map[a.first] > map[b.first]) {
                bool notAnyOne = true;
                int p = a.first, q = b.first;
                for (int i = 0; i < n; ++i, p >>= 1, q >>= 1) {
                    if ((p & 1) && !(q & 1) && map.count(b.first | (1 << i)))
                        notAnyOne = false;
                }
                if (notAnyOne) {
                    cout << "NO";
                    return 0;
                }
            }
        }
    }

    cout << "YES";
}
