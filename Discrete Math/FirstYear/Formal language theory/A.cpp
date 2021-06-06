#include <iostream>
#include <string>
#include <vector>
 
using namespace std;
string s;
vector<vector<pair<char, char>>> table;
 
 
bool check (int ind, char st) {
    if (st == '0')
        return ind == s.size();
    if (ind >= s.size()) return false;
    bool temp = false;
    for (auto & i : table[st - 'A']) {
        if (s[ind] == i.second) {
            temp = temp | check(ind + 1, i.first);
        }
    }
    return temp;
}
 
int main() {
    freopen("automaton.in", "r", stdin);
    freopen("automaton.out", "w", stdout);
    ios::sync_with_stdio(false);
    cin.tie();
    int n;
    char start;
    char tmp;
    cin >> n >> start;
    table.resize(26);
    for (int i = 0; i < n; i++) {
        string s;
        cin >> tmp >> s >> s;
        table[tmp - 'A'].push_back({s.size() == 2 ? s[1] : '0', s[0]});
    }
    int m;
    cin >> m;
    for (int i = 0; i < m; i++) {
        cin >> s;
        if (check(0, start)) cout << "yes" << '\n';
        else cout << "no" << '\n';
    }
}