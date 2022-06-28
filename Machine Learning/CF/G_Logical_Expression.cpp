#include <iostream>
#include <string>
#include <vector>
#include <cmath>
#include <algorithm>
#include <map>
#include <set>
#include <sstream>
#include <optional>

using namespace std;



long one(long x) {
    long res = 0;
    while (x > 0) {
        ++res;
        x = x & (x - 1);
    }
    return res;
}


int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    long m;
    cin >> m;
    cout << "2\n" << (1 << m) << " 1\n";
    for (long i = 0; i < 1 << m; i++) {
        for (long j = 0; j < m; j++) {
            if ((i & (1 << j)) != 0) {
                cout << "1.0 ";
            } else {
                cout  << "-1234567890000.0 ";
            }
        }
        cout << 0.5 - one(i) << '\n';
    }
    for (long i = 0; i < 1 << m; i++) {
        long x;
        cin >> x;
        cout << x << ".0 ";
    }
    cout << -0.5 << '\n';
    cout.flush();
    return 0;
}
