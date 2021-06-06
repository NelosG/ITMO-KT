#include <iostream>
#include <vector>


using namespace std;

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);

    int k;
    cin >> k;

    vector<int> a(k);
    vector<int> c(k);

    for (int i = 0; i < k; ++i)
        cin >> a[i];

    for (int i = 0; i < k; ++i)
        cin >> c[i];

    vector<int> P(k + 1, 0);

    auto f = [&a, &c](int k){
      int value = 0;
      for (int i = 0; i < k; ++i)
          value += c[i] * a[k - i - 1];
      return a[k] - value;
    };

    for (int i = 0; i < k; ++i)
        P[i] = f(i);

    int deg = k + 1;
    while (deg >= 0 && P[deg - 1] == 0)
        --deg;

    cout << deg - 1 << '\n';
    for (int i = 0; i < deg; ++i)
        cout << P[i] << ' ';

    cout << '\n' << k << '\n' << 1 << ' ';
    for (int Ci : c)
        cout << -Ci << ' ';
}