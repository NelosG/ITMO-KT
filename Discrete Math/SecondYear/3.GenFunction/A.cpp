#include <iostream>
#include <vector>
#include <algorithm>

using namespace std;

int MOD = 998244353;

int mod(long long a) {
    return (a + MOD) % MOD;
}

int Mul(long long a, long long b) {
    return mod(a * b);
}

int get(int i, vector<int> & a) {
    return i < a.size() ? a[i] : 0;
}

void print(vector<int> && a, int amount) {
    for (int i = 0; i < amount; i++) {
        cout << get(i, a) << ' ';
    }
    cout << '\n';
}

void print(vector<int> && a) {
    int power = 0;
    for (int i = a.size() - 1; i >= 0; i--) {
        if (a[i] != 0) {
            power = i;
            break;
        }
    }
    cout << power << '\n';
    print(static_cast<vector<int> &&>(a), power + 1);
}

vector<int> sum(vector<int> &P, vector<int> &Q) {
    vector<int> sum(max(P.size(), Q.size()), 0);
    for (int i = 0; i < sum.size(); i++) {
        sum[i] = mod(get(i, P) + get(i, Q));
    }
    return sum;
}

vector<int> prod(vector<int> & P, vector<int> & Q) {
    vector<int> product(P.size() + Q.size(), 0);
    for (int i = 0; i < P.size(); i++) {
        for (int j = 0; j < Q.size(); j++) {
            product[i + j] = mod(product[i + j]+ Mul(P[i], Q[j]));
        }
    }
    return product;
}

vector<int> div(vector<int> & P, vector<int> & Q) {
    int n = 1000;
    vector<int> division(n, 0);
    for (int i = 0; i < n; i++) {
        long bc = 0;
        for (int j = 1; j <= i; j++) {
            bc = mod(bc + Mul(get(j, Q), get(i - j, division)));
        }
        division[i] = mod(mod(get(i, P) - bc) / Q[0]);
    }
    return division;
}

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int n, m;
    cin >> n >> m;
    vector<int> P(n + 1);
    vector<int> Q(m + 1);

    for (int & i : P) {
        cin >> i;
    }

    for (int & i : Q) {
        cin >> i;
    }

    print(sum(P, Q));
    print(prod(P, Q));
    print(div(P, Q), 1000);
}
