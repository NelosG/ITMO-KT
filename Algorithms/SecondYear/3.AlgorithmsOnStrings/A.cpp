#include <iostream>
#include <string>
#include <vector>

using namespace std;

const uint32_t M = 16769023;

const uint32_t x = 61;

vector<uint32_t> pw;

vector<uint32_t> hashes;


uint32_t getHash(size_t l, size_t r) {
    uint32_t a = hashes[r];
    if (a == 0) a = M;
    uint64_t b = hashes[l];
    if (b == 0) b = M;
    uint64_t X = pw[r - l];
    uint64_t bb = (b * X) % M;
    if (a < bb) {
        a += M;
    }
    return (a - bb) % M;
}

int main() {

    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);


    string s;
    int n;
    cin >> s >> n;

    pw.resize(s.length() + 1);
    pw[0] = 1;
    for (int i = 1; i <= s.length(); i++) {
        uint64_t p = (uint64_t)x * (uint64_t)pw[i - 1];
        p %= M;
        pw[i] = p;
    }


    hashes.resize(s.length() + 1);
    hashes[0] = 0;

    for (int i = 1; i <= s.length(); i++) {
        hashes[i] = (hashes[i - 1] * x + (s[i - 1] - 'a' + 1)) % M;
    }

    for (int i = 0; i < n; i++) {
        int a, b, c, d;
        cin >> a >> b >> c >> d;
        if (getHash(a - 1, b) == getHash(c - 1, d)) {
            cout << "Yes\n";
        } else {
            cout << "No\n";
        }
    }
}
