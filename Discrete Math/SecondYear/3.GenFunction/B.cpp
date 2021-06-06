#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;
long MOD = 998244353;

int mod(long long a) {
    return (a + MOD) % MOD;
}

int Mul(long long a, long long b) {
    return mod(a * b);
}

int Pow(long long a, long long b) {
    if (b == 0) {
        return 1;
    }

    long res = Pow(a, b / 2);
    res = Mul(res, res);
    if (b % 2 == 1) {
        res = Mul(res, a);
    }

    return res;
}

int Div(long a, long b) {
    return Mul(a, Pow(b, MOD - 2));
}


void addVec(vector<int> &dist, vector<int> &add) {
    for (int i = 0; i < add.size(); i++) {
        dist[i] = mod(dist[i] + add[i]);
    }
}

long long min(int a, unsigned long long b) {
    if(a > b) {
        return b;
    }
    return a;
}

vector<int> mulVec(vector<int> &first, vector<int> &second, int m) {
    vector<int> result(min(m, first.size() + second.size() - 1), 0);
    for (int i = 0; i < first.size(); i++) {
        for (int j = 0; j < second.size() && i + j < m; j++) {
            result[i + j] = mod(result[i + j] + Mul(first[i], second[j]));
        }
    }
    return result;
}

void print(vector<int> &coefficients) {
    for (int coefficient : coefficients) {
        cout << coefficient << ' ';
    }
}

void SquareRoot(vector<int> &genFunction, int m) {
    if (m == 1) {
        cout <<  "1";
        return;
    }

    vector<int> coefficients(m, 0);
    coefficients[0] = 1;
    vector<int> x(genFunction.size(), 0);
    for (int i = 0; i < x.size(); i++) {
        x[i] = genFunction[i];
    }

    long a = 1, b = 2;
    vector<int> temp(1, Div(a, b));
    temp = mulVec(x, temp, m);

    addVec(coefficients, temp);
    for (int i = 2; i < m; i++) {
        b = Mul(b, 2);
        b = Mul(b, i);
        a = Mul(a, 2 * i - 3);
        long coefficient = Div(a, b);
        if (i % 2 == 0) {
            coefficient = (MOD - coefficient) % MOD;
        }

        x = mulVec(x, genFunction, m);
        temp.assign(1, coefficient);
        temp = mulVec(x, temp, m);
        addVec(coefficients, temp);
    }

    print(coefficients);
}

void Exponent(vector<int> &genFunction, int m) {
    vector<int> temp;
    vector<int> coefficients(m, 0);
    coefficients[0] = 1;
    vector<int> x(genFunction.size(), 0);
    x[0] = 1;
    long b = 1;
    for (int i = 1; i < m; i++) {
        b = Mul(b, i);
        x = mulVec(x, genFunction, m);
        temp.assign(1, Div(1L, b));
        temp = mulVec(x, temp, m);
        addVec(coefficients, temp);
    }

    print(coefficients);
}

void Logarithm(vector<int> &genFunction, int m) {
    vector<int> temp;
    vector<int> coefficients(m, 0);
    vector<int> x(genFunction.size(), 0);
    x[0] = 1;

    for (int i = 1; i < m; i++) {
        x = mulVec(x, genFunction, m);
        long coefficient = Div(1L, i);
        if (i % 2 == 0) {
            coefficient = (MOD - coefficient) % MOD;
        }
        temp.assign(1, coefficient);
        temp = mulVec(x, temp, m);
        addVec(coefficients, temp);
    }

    print(coefficients);
}

int main() {
    int n, m;
    cin >> n >> m;
    vector<int> P(n + 1);

    for (int &i : P) {
        cin >> i;
    }

    SquareRoot(P, m);
    cout << '\n';
    Exponent(P, m);
    cout << '\n';
    Logarithm(P, m);
    cout << '\n';
}
