#include <iostream>
#include<stdio.h>
#include<vector>
#include <algorithm>

#pragma warning(disable : 4996);
using namespace std;
int len = 524288;
int MAX = -1;
int ind;
int ind2;
struct Y;
struct X;

void build(int v);

bool comp(const X &a, const X &b);

int max(int a, int b);

int mix(int v, int l, int r, int a, int b);

void update(int v, int l, int r, int a, int b, int x);

void upd(int v);

void push(int v);

struct Y {
    Y(int val, int in) {
        this->val = val;
        this->in = in;
    }

    Y() {
        this->val = 0;
        this->in = 0;
    }

    int in;
    int val;
};

Y *tree = new Y[2 * len - 1];
int *dop = new int[2 * len - 1];

struct X {
    X(int a, int b1, int b2, int c) {
        this->x = a;
        this->y1 = b1;
        this->y2 = b2;
        this->c = c;

    }

    X() {
        this->x = 0;
        this->y1 = 0;
        this->y2 = 0;
        this->c = 0;
    }

    int x;
    int y1;
    int y2;
    int c;
};

void build(int v, int l, int r) {
    if (v < len - 1) {
        build(2 * v + 1, l, (l + r) / 2);
        build(2 * v + 2, (l + r) / 2, r);
    }
    tree[v].in = l;
}

bool comp(const X &a, const X &b) {
    if (a.x == b.x) {
        return a.c > b.c;
    }
    return a.x < b.x;
}

int max(int a, int b) {
    if (a > b) return a;
    return b;
}

int mix(int v, int l, int r, int a, int b) {
    if (l >= b || a >= r) return -2000000;
    if (a <= l && r <= b) return tree[v].val;
    return max(mix(2 * v + 1, l, (l + r) / 2, a, b), mix(2 * v + 2, (l + r) / 2, r, a, b));
}

void update(int v, int l, int r, int a, int b, int x) {
    push(v);
    if (l >= b || a >= r) return;
    if (a <= l && r <= b) {
        dop[v] += x;
        return;
    }
    update(2 * v + 1, l, (l + r) / 2, a, b, x);
    update(2 * v + 2, (l + r) / 2, r, a, b, x);
    upd(v);
}

void upd(int v) {
    push(v);
    if (v < len - 1) {
        push(2 * v + 1);
        push(2 * v + 2);
        if (tree[2 * v + 1].val > tree[2 * v + 2].val) {
            tree[v].val = tree[2 * v + 1].val;
            tree[v].in = tree[2 * v + 1].in;
        } else {
            tree[v].val = tree[2 * v + 2].val;
            tree[v].in = tree[2 * v + 2].in;
        }
    }
}

void push(int v) {
    if (dop[v] != 0) {
        tree[v].val += dop[v];
        if (v < len - 1) {
            dop[2 * v + 1] += dop[v];
            dop[2 * v + 2] += dop[v];
        }
        dop[v] = 0;
    }
}


int main() {
    int n;
    scanf("%i", &n);
    vector<X> m;
    m.resize(2 * n);
    int x1, x2, y1, y2;
    for (int i = 0; i < 2 * len - 1; i++) {
        dop[i] = 0;
    }
    build(0, 0, len);
    for (int i = 0; i < n; i++) {
        scanf("%i %i %i %i", &x1, &y1, &x2, &y2);
        m.push_back(X(x1, y1, y2, 1));
        m.push_back(X(x2, y1, y2, -1));
    }
    sort(m.begin(), m.end(), comp);
    for (int i = 0; i < (int) m.size(); i++) {
        update(0, 0, len, m[i].y1 + 200000, m[i].y2 + 200001, m[i].c);
        if (tree[0].val > MAX) {
            MAX = tree[0].val;
            ind = tree[0].in - 200000;
            ind2 = i;
        }
    }
    printf("%i\n%i %i", MAX, m[ind2].x, ind);
    return 0;
}
