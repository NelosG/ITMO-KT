#include <fstream>

#define Debug
#ifdef Debug

#include <iostream>

#endif


using namespace std;

int main() {
#ifndef Debug
    ifstream cin("multispan.in");
    ofstream cout("multispan.out");
#endif

    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);
    int i = 0, j = 1;
    i^=1;
    j^=1;
    cout << i << " " << j << '\n';
    i^=1;
    j^=1;
    cout << i << " " << j << '\n';
}
