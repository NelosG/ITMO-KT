#include <iostream>
#include <vector>
#include <string>
#include <algorithm>
using namespace std;


int main(){
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    string p;
    string t;
    cin >> p >> t;
    string s = p +'#'+t;
    int n = s.length();
    vector<int> zf(s.length() + 1, 0);
    int left = 0, right = 0;
    for (int i = 1; i < s.length(); i++) {
        zf[i] = max(0, min(right - i, zf[i - left]));
        while (i + zf[i] < s.length() && s[zf[i]] == s[i + zf[i]])
            zf[i]++;
        if (i + zf[i] > right) {
            left = i;
            right = i + zf[i];
        }
    }

    vector<int> result;
    for (int i = p.size(); i <n; i++){
        if (zf[i]== p.size()){
            result.push_back(i- p.size());
        }
    }
    cout << result.size() << "\n";
    for (auto i : result){
        cout << i << " ";
    }
}
