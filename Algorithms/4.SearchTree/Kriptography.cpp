#include <iostream>
#include <fstream>

#pragma warning(disable : 4996);
using namespace std;
int r, n, m;
FILE *f = fopen("crypto.out", "w");
FILE *ff = fopen("crypto.in", "r");

int mod(int a, int b);

struct M {
    M() {
        this->a = 0;
        this->b = 0;
        this->c = 0;
        this->d = 0;
    }

    M(int a, int b, int c, int d) {
        this->a = a;
        this->b = b;
        this->c = c;
        this->d = d;
    }

    void mul(M A, M B) {
        a = (mod(A.a * B.a + A.b * B.c, r));
        b = (mod(A.a * B.b + A.b * B.d, r));
        c = (mod(A.c * B.a + A.d * B.c, r));
        d = (mod(A.c * B.b + A.d * B.d, r));
    }

    void print() {
        fprintf(f, "%i %i\n%i %i\n\n", a, b, c, d);
        //out << a << " " << b << endl << c << " " << d << endl << endl;
    }

    int a;
    int b;
    int c;
    int d;
};

M E = M(1, 0, 0, 1);

M sum(int v, int l, int r, int a, int b);

M *ar;

int main(void) {
    //out.open("crypto.out");
    //ifstream in("crypto.in");
    fscanf(ff, "%i %i %i", &r, &n, &m);
    //in >> r >> n >> m;
    int one = 1;
    while (one < n) {
        one *= 2;
    }
    ar = new M[2 * one - 1];
    for (int i = one - 1; i < one - 1 + n; i++) {
        int a, b, c, d;
        fscanf(ff, "%i %i %i %i", &a, &b, &c, &d);
        //in >> a >> b >> c >> d;
        ar[i] = M(a, b, c, d);
    }
    for (int i = one - 1 + n; i < one * 2 - 1; i++) {
        ar[i] = E;
    }
    for (int i = one - 2; i >= 0; i--) {
        ar[i].mul(ar[2 * i + 1], ar[2 * i + 2]);
    }
    for (int i = 0; i < m; i++) {
        int l, r;
        fscanf(ff, "%i %i", &l, &r);
        //in >> l >> r;
        sum(0, 0, one, l - 1, r).print();
    }
    //in.close();
    //out.close();
    return 0;
}

int mod(int a, int b) {
    return (a % b + b) % b;
}

M sum(int v, int l, int r, int a, int b) {
    if (b <= l || a >= r) return E;
    if (b >= r && a <= l) {
        return ar[v];
    }
    M temp;
    temp.mul(sum(2 * v + 1, l, (l + r) / 2, a, b), sum(2 * v + 2, (l + r) / 2, r, a, b));
    return temp;
}
