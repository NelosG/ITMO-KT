#include <iostream>
#include <stdio.h>
#include <string>
#include <vector>
#include <cmath>
 
using namespace std;
 
class HashTable;
 
class HashTable {
    int mas = 10000;
    int *T = new int[mas];
    vector<vector<vector<int>>> table;
    vector<int> lastPair;
    int lastHash = 0;
 
    int get(int key) {
        if (contains(key))
            return lastPair[1];
        return NULL;
    }
 
    void createTable() {
 
        table.resize(mas+1, vector<vector<int>>());
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
 
 
    int h(int i) {
        int hash = (i % mas + mas) % mas;
        while (i > 0) {
            hash = T[(hash + i) % mas];
            i /= 10;
        }
        return hash;
    }
 
 
public:
    void put(int key, int value) {
        if (!contains(key)) {
            vector<int> temp;
            temp.push_back(key);
            temp.push_back(value);
            table[lastHash].push_back(temp);
        }
    }
 
    void remove(int key) {
        lastHash = h(key);
        for (int i = 0; i < table[lastHash].size(); i++) {
            if (table[lastHash][i][0] == key) {
                table[lastHash].erase(table[lastHash].begin() + i);
                break;
            }
        }
    }
 
    bool contains(int key) {
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
};
 
int main() {
    HashTable table;
    FILE *in;
    in = fopen("set.in", "r");
    FILE *out;
    out = fopen("set.out", "w");
    char* str;
    while (!feof(in)) {
        str = (char*)(malloc(sizeof(char) * 6));
        int key;
        fscanf(in, "%s%i", str, &key);
        if (str[0] == 'i') {
            table.put(key, 0);
        } else if (str[0] == 'd') {
            table.remove(key);
        } else if (str[0] == 'e') {
            if (table.contains(key)) fprintf(out, "true\n");
            else fprintf(out, "false\n");
        }
    }
}