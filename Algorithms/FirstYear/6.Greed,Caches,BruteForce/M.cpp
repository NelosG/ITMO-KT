#include <iostream>
#include <stdio.h>
#include <string>
#include <utility>
#include <vector>
#include <cmath>
 
using namespace std;
 
class HashTable;
 
class HashTable {
    int mas = 10000;
    int *T = new int[mas];
    vector<vector<vector<string>>> table;
    vector<string> lastPair;
    int lastHash = 0;
 
    void createTable() {
 
        table.resize(mas+1, vector<vector<string>>());
        for (int i = 0; i < mas; i++) {
            T[i] = i;
        }
        int a, b, temp;
        for (int i = 0; i < mas; i++) {
            a = abs(rand() % mas);
            b = abs(rand() % mas);
            temp = T[a];
            T[a] = T[b];
            T[b] = temp;
        }
    }
 
 
    int h(string s) {
            int hash =  s.length() % mas;
            for (char i : s) {
                hash = T[(hash + (int) i) % mas];
            }
            return hash;
    }
 
public:
    void put(string key, string value) {
        if (!contains(key)) {
            vector<string> temp;
            temp.push_back(key);
            temp.push_back(value);
            table[lastHash].push_back(temp);
        } else {
            for(int i = 0; i < table[lastHash].size();i++) {
                if(table[lastHash][i][0] == key)table[lastHash][i][1] = value;
            }
        }
    }
 
    void remove(string key) {
        lastHash = h(key);
        for (int i = 0; i < table[lastHash].size(); i++) {
            if (table[lastHash][i][0] == key) {
                table[lastHash].erase(table[lastHash].begin() + i);
                break;
            }
        }
    }
 
    bool contains(string key) {
        lastHash = h(key);
        for (auto &i : table[lastHash]) {
            if (i[0] == key) {
                lastPair = i;
                return true;
            }
        }
        return false;
    }
 
    HashTable() {
        createTable();
    }
 
    string get(string key) {
        if (contains(key))
            return lastPair[1];
        return "none";
    }
};
 
int main() {
    HashTable table;
    ios_base::sync_with_stdio(false);
    freopen("map.in", "r", stdin);
    freopen("map.out", "w", stdout);
    FILE *out;
    out = fopen("map.out", "w");
    while (cin) {
        string str, key;
        cin >> str >> key;
        if (str[0] == 'p') {
            string s;
            cin >> s;
            table.put(key, s);
        } else if (str[0] == 'd') {
            table.remove(key);
        } else if (str[0] == 'g') {
            fprintf(out, "%s\n", table.get(key).c_str());
//        cout << table.get(key) << "\n";
        }
    }
}