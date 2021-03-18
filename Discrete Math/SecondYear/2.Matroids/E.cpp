#include <vector>
#include <algorithm>
#include <unordered_set>
#include <fstream>

using namespace std;

#define Debug
#ifdef Debug

#include <iostream>

#endif

int n;


void recursive_insert(unsigned int num, unsigned int st, unordered_set<unsigned int> &cycles) {
    if (cycles.count(num))
        return;

    for (unsigned int i = st; i < n; i++)
        if ((num & (1 << i)) == 0)
            recursive_insert(num | (1 << i), i + 1, cycles);

    cycles.insert(num);
}

int main() {

#ifndef Debug
    std::ifstream cin("cycles.in");
    std::ofstream cout("cycles.out");
#endif

    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    vector<pair<int, unsigned int>> weIndex;
    unordered_set<unsigned int> cycles;

    int m;
    cin >> n >> m;
    weIndex.resize(n);

    for (int i = 0, in; i < n; i++) {
        cin >> in;
        weIndex.emplace_back(in, i);
    }

    sort(weIndex.begin(), weIndex.end(),
         [](auto const &a, auto const &b) { return a.first > b.first; });

    for (unsigned int i = 0, count, num; i < m; i++) {
        cin >> count;
        num = 0;
        for (unsigned int j = 0, in; j < count; j++) {
            cin >> in;
            num |= (1 << (in - 1));
        }
        recursive_insert(num, 0, cycles);
    }

    unsigned int result = 0;

    for (auto&[a, i] : weIndex)
        if (!cycles.count(result | (1 << i)))
            result |= (1 << i);



    long long sum = 0;

    for (auto&[a, i] : weIndex)
        if ((result & (1 << i)) != 0)
            sum += a;

    cout << sum;

    return 0;
}
