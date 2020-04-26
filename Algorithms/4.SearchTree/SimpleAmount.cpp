#include <iostream>

using namespace std;

int main() {
    int n = 0;
    int x = 0;
    int y = 0;
    long a0 = 0;
    int m = 0;
    long long sum = 0;
    int z = 0;
    int t = 0;
    int b0 = 0;
    long dwa30 = 1073741824;
    long dwa16 = 65536;
    cin >> n >> x >> y >> a0 >> m >> z >> t >> b0;
    int *b = new int[20000000];
    int *c = new int[20000000];
    b[0] = b0;
    c[0] = (b0 % n + n) % n;
    for (int i = 1; i < 2 * m; i++) {
        b[i] = ((b[i - 1] * z + t) % dwa30 + dwa30) % dwa30;
        c[i] = b[i] % n;
    }
    delete b;
    long long *pref = new long long[10000000];
    pref[0] = a0;
    for (int i = 1; i < n; i++) {
        pref[i] = (pref[i - 1] * x + y) % dwa16;
    }
    for (int i = 1; i < n; i++) {
        pref[i] += pref[i - 1];
    }
    for (int i = 0; i < m; i++) {
        int l = c[2 * i];
        int r = c[2 * i + 1];
        if (l < r) {
            if (l < 0) l = 0;
            if (l != 0) {
                sum -= pref[l - 1];
            }
            sum += pref[r];
        } else {
            if (r < 0) r = 0;
            if (r != 0) {
                sum -= pref[r - 1];
            }
            sum += pref[l];
        }
    }
    cout << sum;
    return 0;

}