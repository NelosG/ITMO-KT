#include<stdio.h>
#include<iostream>
#include<string>

#pragma warning(disable : 4996);
using namespace std;
long *t;
long *dop;
int one = 1;
const long lmv = 10000001ll;
long minx = lmv;
int k = -1;
bool flag = true;

void find(int v);

long minn(long a, long b);

long min(int v, int l, int r, int a, int b);

void upd(int v);

void push(int v);

void update(int v, int l, int r, int a, int b, int x);

void wall(int v, int l, int r, int a, int b);

long minn(long a, long b) {
    if (a < b) return a;
    return b;
}

long min(int v, int l, int r, int a, int b) {
    push(v);
    if (l >= b || r <= a) return lmv;
    if (l >= a && r <= b) {
        if (t[v] < minx) {
            minx = t[v];
        }
        return t[v];
    }
    push(2 * v + 1);
    push(2 * v + 2);
    return minn(min(2 * v + 1, l, (r + l) / 2, a, b),
                min(2 * v + 2, (r + l) / 2, r, a, b));
}

void find(int v) {
    push(v);
    if (v < one - 1) {
        push(2 * v + 1);
        push(2 * v + 2);
        if (t[2 * v + 1] <= minx) {
            find(2 * v + 1);
        } else {
            if (t[2 * v + 2] <= minx) {
                find(2 * v + 2);
            } else {
                return;
            }
        }
    } else {
        if (t[v] == minx) {
            k = v;
            flag = false;
        }
    }
}

void wall(int v, int l, int r, int a, int b) {
    if (flag) {
        push(v);
        if (l >= b || r <= a) return;
        if (l >= a && r <= b) {
            if (v < one - 1) {
                find(v);
            } else {
                if (t[v] == minx) {
                    k = v;
                    flag = false;
                }
            }
            return;
        }
        wall(2 * v + 1, l, (r + l) / 2, a, b);
        wall(2 * v + 2, (r + l) / 2, r, a, b);
    }
}

void update(int v, int l, int r, int a, int b, int x) {
    push(v);
    if (l >= b || r <= a) return;
    if (l >= a && r <= b) {
        if (t[v] < x) {
            dop[v] = x;
        }
        return;
    }
    update(2 * v + 1, l, (r + l) / 2, a, b, x);
    update(2 * v + 2, (r + l) / 2, r, a, b, x);
    upd(v);
}

void upd(int v) {
    push(v);
    if (v < one - 1) {
        push(2 * v + 1);
        push(2 * v + 2);
        t[v] = minn(t[2 * v + 1], t[v * 2 + 2]);
    }
}


void push(int v) {
    if (dop[v] != lmv) {
        if (v < one - 1) {
            push(2 * v + 1);
            push(2 * v + 2);
            if (t[v * 2 + 1] < dop[v]) {
                dop[2 * v + 1] = dop[v];
            }
            if ((t[v * 2 + 2]) < dop[v]) {
                dop[2 * v + 2] = dop[v];
            }
        }
        if (t[v] < dop[v]) {
            t[v] = dop[v];
        }
        dop[v] = lmv;
    }
}

int main(void) {
    int n, m;
    scanf("%i %i", &n, &m);
    while (one < n) {
        one *= 2;
    }
    t = new long[one * 2 - 1];
    dop = new long[one * 2 - 1];

    for (int i = 0; i < one * 2 - 1; i++) {
        if (i < one - 1 + n) {
            t[i] = 0;
        } else {
            t[i] = -100;
        }
        dop[i] = lmv;
    }
    string s;
    for (int i = 0; i < m; i++) {
        cin >> s;
        if (s == ("defend")) {
            int a, b, c;
            scanf("%i %i %i", &a, &b, &c);
            update(0, 0, one, a - 1, b, c);

        } else {
            int d, e;
            scanf("%i %i", &d, &e);
            minx = min(0, 0, one, d - 1, e);
            wall(0, 0, one, d - 1, e);
            printf("%i %i\n", minx, k - one + 2);
            k = -1;
            flag = true;
            minx = lmv;

        }
    }
    return 0;
}