#include <iostream>
#include <vector>
#include <algorithm>
 
using namespace std;
void print(vector<int> *a);
bool check(vector<int> *a, vector<int> *res) {
    vector<int> c;
    for (int j = 0; j < 16; j++) {
        c.push_back(0);
    }
    int temp;
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            temp = 0;
            for(int k = 0; k < 4; k++) {
                temp += (*a)[i*4 + k] * (*a)[k*4 + j];
            }
            c[i * 4 + j] = temp % 2;
        }
    }
    bool flag = false;
    for (int j = 0; j < 4; j++) {
        for (int i = 0; i < 4; i++) {
            if(c[i * 4 + j] % 2 != (*res)[i * 4 + j]) {
                flag = true;
                break;
            }
        }
        if(flag) break;
    }
    return !flag;
}
 
bool next(vector<int> *a) {
    int i = 0;
    bool flag = false;
    for (int k = 0; k < (*a).size(); k++) {
        if ((*a)[k] == 0) {
            flag = true;
            break;
        }
    }
    if (flag) {
        while ((*a)[i] == 1) {
            (*a)[i] = 0;
            i++;
        }
        (*a)[i] = 1;
        return true;
    }
    return false;
}
 
void print(vector<int> *a) {
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            cout << (*a)[i*4 + j] << " ";
        }
        cout << "\n";
    }
}
 
int main() {
    freopen("sqroot.in", "r", stdin);
    freopen("sqroot.out", "w", stdout);
    ios::sync_with_stdio(false);
    vector<int> in;
    vector<int> a;
    int temp;
    for (int i = 0; i < 16; i++) {
        a.push_back(0);
        in.push_back(0);
    }
    for (int i = 0; i < 4; i++) {
        for (int j = 0; j < 4; j++) {
            cin >> temp;
            in[i*4 + j] = temp;
        }
    }
    bool flag = true;
    if(check(&a, &in)) {
        print(&a);
        flag = false;
    }
    while (next(&a)) {
        if(check(&a, &in)) {
            print(&a);
            flag = false;
            break;
        }
    }
    if(flag) cout << "NO SOLUTION";
    return 0;
}