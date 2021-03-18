
#include <iostream>
#include <cmath>

using namespace std;

int minn(int a, int b);

int min(int a, int b);

int max(int a, int b);

int pow(int x);

int fl(int len);

int *ar;
int **st;

int main(void) {
    int n, m, a0, u, v;
    cin >> n >> m >> a0 >> u >> v;
    int log = -1;
    int temp = n;
    while (temp > 0) {
        temp /= 2;
        log++;
    }
    ar = new int[n];
    st = new int *[n];
    ar[0] = a0;
    for (int i = 0; i < n - 1; i++) {
        ar[i + 1] = (23 * ar[i] + 21563) % 16714589;
    }
    for (int i = 0; i < n; i++) {
        st[i] = new int[log];
        st[i][0] = ar[i];
        for (int j = 1; j < log; j++) {
            st[i][j] = 0;
        }
    }
    for (int j = 1; j <= log; j++) {
        for (int i = 0; i + pow(j) <= n; i++) {
            st[i][j] = minn(st[i][j - 1], st[i + pow(j - 1)][j - 1]);
        }
    }

    for (int i = 0; i < m - 1; i++) {
        int ri = min(minn(u, v) - 1, max(u, v) - 1);
        u = ((17 * u + 751 + ri + 2 * (i + 1)) % n) + 1;
        v = ((13 * v + 593 + ri + 5 * (i + 1)) % n) + 1;
    }
    cout << u << " " << v << " " << min(minn(u, v) - 1, max(u, v) - 1);
}

int minn(int a, int b) {
    if (a < b) return a;
    return b;
}

int max(int a, int b) {
    if (a > b) return a;
    return b;
}

int pow(int x) {
    int temp = 1;
    for (int i = 0; i < x; i++) {
        temp *= 2;
    }
    return temp;
}

int fl(int len) {
    if (len == 1) return 0;
    return fl(len / 2) + 1;
}


int min(int a, int b) {
    int temp = fl(b - a + 1);
    return minn(st[a][temp], st[b + 1 - pow(temp)][temp]);
}