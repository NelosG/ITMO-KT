#include <algorithm>
#include <iostream>
#include <string>
#include <vector>

using namespace std;

using Long = long long;

const int ELEMENTS_COUNT = 7;
const int OFFSET = 2;


pair<string, string> parse(string &expression) {
    int balance = 0, cut;
    for (int i = 0; i < expression.length(); ++i) {
        switch (expression[i]) {
            case '(':
                ++balance;
                break;
            case ')':
                --balance;
                break;
            case ',':
                if (balance == 1) {
                    cut = i;
                    return {expression.substr(OFFSET, cut),
                            expression.substr(cut + 1, expression.length() - 1)};
                }
        }
    }
    return {"Hello", "World"}; // never returns
}
Long C(Long k, Long n) {
    Long res = 1;
    for (Long i = k + 1; i <= n; ++i) {
        res *= i;
    }
    return res;
}

vector<Long> calculate(string &expression);

vector<Long> calculate(string &&expression) {
        return calculate(expression);
}

vector<Long> calculateSL(string &expression) {
    char object = expression[0];

    vector<Long> weights{1, 0, 0, 0, 0, 0, 0};
    vector<Long> inner = calculate(expression.substr(OFFSET, expression.length() - 1));

    if (object == 'L') {
        for (int w = 1; w < ELEMENTS_COUNT; ++w)
            for (int k = 1; k <= w; ++k)
                weights[w] += inner[k] * weights[w - k];
    } else {
        vector<vector<Long>> matrix(ELEMENTS_COUNT, vector<Long>(ELEMENTS_COUNT, 0));
        for (int i = 0; i < ELEMENTS_COUNT; ++i) {
            matrix[0][i] = 1;
        }
        Long n;
        for (int i = 1; i < ELEMENTS_COUNT; ++i) {
            for (int j = 1; j < ELEMENTS_COUNT; ++j) {
                for (int k = 0; k <= i / j; ++k) {
                    n = inner[j] + k - 1;
                    n = n < 0 ? 0 : n;
                    matrix[i][j] += matrix[i - j * k][j - 1] *
                                    C(n - k, n) /
                                    C(1, k);
                }
            }
            weights[i] = matrix[i][i];
        }
    }
    return weights;
}


vector<Long> calculate(string &expression) {
    char object = expression[0];

    if (object == 'B') return vector<Long>{0, 1, 0, 0, 0, 0, 0};
    if (object == 'L' || object == 'S') return calculateSL(expression);

    vector<Long> weights{0, 0, 0, 0, 0, 0, 0};
    {
        pair<string, string> arguments = parse(expression);
        vector<Long> left = calculate(arguments.first);
        vector<Long> right = calculate(arguments.second);
        for (int w = 0; w < ELEMENTS_COUNT; ++w)
            for (int k = 0; k <= w; ++k)
                weights[w] += left[k] * right[w - k];
    }
    return weights;
}


int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    string s;
    cin >> s;
    for (Long l : calculate(s)) {
        cout << l << ' ';
    }
}
