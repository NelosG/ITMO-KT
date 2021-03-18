#include <vector>
#include <set>
#include <map>
#include <fstream>

#define Debug
#ifdef Debug

#include <iostream>

#endif
using namespace std;

int main() {
#ifndef Debug
    ifstream cin("schedule.in");
    ofstream cout("schedule.out");
#endif
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);


    int n;
    cin >> n;
    map<long, vector<long>> tasks;
    for (int i = 0; i < n; i++) {
        long a, b;
        cin >> a >> b;
        tasks[a].push_back(b);
    }

    long long sum = 0;
    multiset<long> cur_tasks;

    for (const auto &pair : tasks) {
        long time = pair.first;
        for (auto t : pair.second) {
            cur_tasks.insert(t);
        }
        while (cur_tasks.size() > time) {
            sum += *cur_tasks.begin();
            cur_tasks.erase(cur_tasks.begin());
        }
    }
    cout << sum;
}
