#include <iostream>
#include <string>

using namespace std;
long one = 1;
long long *t;
long long *dop;
long long *sett;

long long min(int v, int l, int r, int a, int b);

long long minn(long long a, long long b);

void set(int v, int l, int r, int a, int b, long long x);

void add(int v, int l, int r, int a, int b, long long x);

void pushset(int v);

void pushadd(int v);

void upd(int v);

int main(void) {
    int n;
    cin >> n;
    long long LMIN = -9223372036854775808ll;
    long long LMAX = 9223372036854775807ll;
    while (one < n) {
        one *= 2;
    }
    t = new long long[2 * one - 1];
    dop = new long long[2 * one - 1];
    sett = new long long[2 * one - 1];
    for (int i = 0; i < 2 * one - 1; i++) {
        t[i] = 0;
        sett[i] = LMIN;
        dop[i] = 0;
    }
    for (int i = one - 1; i < one - 1 + n; i++) {
        cin >> t[i];
    }
    for (int i = one - 1 + n; i < 2 * one - 1; i++) {
        t[i] = LMAX;
    }
    for (int i = one - 2; i >= 0; i--) {
        t[i] = minn(t[2 * i + 1], t[2 * i + 2]);
    }
    while (cin) {
        string s;
        cin >> s;
        if (s == "set") {
            int l;
            int r;
            long long x;
            cin >> l >> r >> x;
            l--;
            set(0, 0, one, l, r, x);

        } else {
            if (s == "min") {
                int l;
                int r;
                cin >> l >> r;
                l--;
                long long temp = min(0, 0, one, l, r);
                cout << temp << endl;
            } else {
                int l;
                int r;
                long long x;
                cin >> l >> r >> x;
                l--;
                add(0, 0, one, l, r, x);

            }
        }

    }
}

long long minn(long long a, long long b) {
    if (a < b) return a;
    return b;
}


void set(int v, int l, int r, int a, int b, long long x) {
    pushadd(v);
    if (b <= l || a >= r) return;
    if (b >= r && a <= l) {
        sett[v] = x;
        return;
    }
    set(2 * v + 1, l, (l + r) / 2, a, b, x);
    set(2 * v + 2, (l + r) / 2, r, a, b, x);
    upd(v);
    return;
}

long long min(int v, int l, int r, int a, int b) {
    pushset(v);
    if (b <= l || a >= r) return 9223372036854775807l;
    if (b >= r && a <= l) {
        return t[v];
    }
    return minn(min(2 * v + 1, l, (l + r) / 2, a, b), min(2 * v + 2, (l + r) / 2, r, a, b));
}

void add(int v, int l, int r, int a, int b, long long x) {
    pushset(v);
    if (b <= l || a >= r) return;
    if (b >= r && a <= l) {
        dop[v] = x;
        return;
    }
    add(2 * v + 1, l, (l + r) / 2, a, b, x);
    add(2 * v + 2, (l + r) / 2, r, a, b, x);
    upd(v);
    return;
}


void pushset(int v) {
    if (sett[v] != -9223372036854775808ll) {
        t[v] = sett[v];
        dop[v] = 0;
        if (v < one - 1) {
            sett[2 * v + 1] = sett[v];
            sett[2 * v + 2] = sett[v];
            dop[2 * v + 1] = 0;
            dop[2 * v + 2] = 0;
            sett[v] = -9223372036854775808ll;
        } else {
            sett[v] = -9223372036854775808ll;
        }
    } else {
        pushadd(v);
    }
}

void pushadd(int v) {
    if (sett[v] == -9223372036854775808ll) {
        if (dop[v] != 0) {
            t[v] += dop[v];
            if (v < one - 1) {
                pushset(2 * v + 1);
                pushset(2 * v + 2);
                dop[2 * v + 1] += dop[v];
                dop[2 * v + 2] += dop[v];
                dop[v] = 0;
            } else {
                dop[v] = 0;
            }
        }
    } else {
        pushset(v);
    }
}

void upd(int v) {
    pushset(v);
    pushset(2 * v + 1);
    pushset(2 * v + 2);
    t[v] = minn(t[2 * v + 1], t[2 * v + 2]);
}