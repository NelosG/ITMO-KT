#include <functional>
#include <iostream>
#include <vector>

using namespace std;
using Long = long long;

int MOD = 104857601;

Long mod(Long a) {
    return (a + MOD) % MOD;
}

Long Mul(Long a, Long b) {
    return mod(a * b);
}

Long get(vector<Long> &a, int i) {
    return 0 <= i && i < a.size() ? mod(a[i]) : 0;
}

vector<Long> evenOdd(vector<Long> &a, int even) {
    vector<Long> ret(a.size() / 2);
    for (int i = 0; i < ret.size(); i++) {
        ret[i] = get(a, 2 * i + even);
    }
    return ret;
}


vector<Long> prod(vector<Long> &P, vector<Long> &Q) {
    vector<Long> product(P.size() + Q.size(), 0);
    for (int i = 0; i < P.size(); i++)
        for (int j = 0; j < Q.size(); j++)
            product[i + j] = mod(product[i + j] + Mul(P[i], Mul(Q[j], j % 2 == 0 ? 1 : -1)));
    return product;
}


int main() {
    Long k, n;
    cin >> k >> n;
    --n;
    vector<Long> a(k, 0);
    for (int i = 0; i < k; cin >> a[i++])
        ;
    vector<Long> c(k, 0);
    for (int i = 0; i < k; cin >> c[i++])
        ;
    vector<Long> q(k + 1, 0);

    q[0] = 1;
    for (int i = 1; i <= k; i++) {
        q[i] = -c[i - 1];
    }
    vector<Long> p(a.size(), 0);
    for (int i = 0; i < a.size(); i++) {
        for (int j = 1; j <= a.size(); j++)
            p[i] = mod(p[i] + Mul(get(c, j - 1), get(a, i - j)));
        p[i] = mod(a[i] - p[i]);
    }

    while (n != 0) {
        vector<Long> temp = prod(q, q);
        p = prod(p, q);
        p = evenOdd(p, n % 2);
        q = evenOdd(temp, 0);
        n /= 2;
    }
    cout << mod(get(p, 0));
}