#define _CRT_DISABLE_PERFCRIT_LOCKS
#include <iostream>
#include <list>
#include <vector>
#include <map>

using namespace std;

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);


    int n, m, k;
    list<int> input;
    cin >> n >> m >> k;
    for (int i = 0, temp ; i < n ; i++) {
        input.push_back((cin >> temp, temp));
    }
    int it = 1;
    map<int, vector<int>> map_numbers;
    for (auto key: input) {
        map_numbers[key].push_back(it++);
    }

    vector<list<int>> answer(k);
    it = 0;
    for (const auto &keyValue: map_numbers) {
        for (const auto &number: keyValue.second) {
            answer[it++].push_back(number);
            it = it % k;
        }
    }
    for (auto &term: answer) {
        cout << term.size() << ' ';
        for (auto &numb: term) {
            cout << numb << ' ';
        }
        cout << '\n';
    }
    cout.flush();
    return 0;
}
