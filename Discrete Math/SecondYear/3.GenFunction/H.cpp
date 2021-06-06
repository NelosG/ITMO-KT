#include <array>
#include <cmath>
#include <iostream>
#include <vector>


using namespace std;

using Long = long long;

constexpr Long MOD = 998244353;

constexpr Long mod(Long a) {
    return (a + MOD) % MOD;
}

template<typename Function, size_t... Indices>
constexpr auto make_array_helper(Function f, std::index_sequence<Indices...>) {
    return std::array<std::result_of_t<Function(size_t)>, sizeof...(Indices)>{f(Indices)...};
}

template<int N, typename Function>
constexpr auto make_array(Function f) {
    return make_array_helper(f, std::make_index_sequence<N>{});
}

constexpr long long generator(size_t index) {
    if (index == 0) {
        return 1;
    }
    return mod(index * generator(index - 1));
}


constexpr int MAXN = 5010;

array<Long, 5010>  fact = make_array<MAXN>(generator);




inline Long Pow(Long a, Long n) {
    Long u = a, res = 1;
    while (n) {
        if (n % 2 == 1)
            res = mod(res * u);
        u = mod(u * u);
        n /= 2;
    }
    return res;
}

inline Long Div(Long a, Long b) {
    return mod(a * Pow(b, MOD - 2));
}

inline Long C(int n, int m) {
    if (n < m)
        return 0ll;
    return Div(fact[n], mod(fact[m] * fact[n - m]));
}

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    fact[0] = 1ll;
    for (Long i = 1ll; i < (Long)MAXN; i++)
        fact[i] = mod(fact[i - 1] * i);

    int n, k;
    cin >> n >> k;

    vector <Long> a(k, 0), b(k, 0);
    for (int i = n - 2, j = 0; i >= j; i--, j++) {
        a[j] = (mod(C(i, j) * pow(-1, j)));
    }
    for (int i = n - 1, j = 0; i >= j; i--, j++) {
        b[j] = (mod(C(i, j) * pow(-1, j)));
    }


    vector <Long> div(k);
    div[0] = Div(a[0], b[0]);
    for (int i = 1; i < k; i++) {
        Long temp = 0;
        for (int j = 1; j <= i; j++)
            temp = mod(temp + mod(b[j] * div[i - j]));
        div[i] = mod(a[i] - temp);
    }

    for (int i = 0; i < k; i++)
        cout << div[i] << '\n';

    return 0;
}