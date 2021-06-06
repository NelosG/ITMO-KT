#include <algorithm>
#include <iostream>
#include <sstream>
#include <vector>

using namespace std;
using Long = long long;

class AnswerPair {
public:
  int value;
  vector<int> indexes;
  string to_str() {
    stringstream ss;
    ss << value << '\n';
    for (int i = 0; i < indexes.size(); ++i) {
      ss << i + 1 << ' ' << indexes[i] << '\n';
    }
    return ss.str();
  }
  AnswerPair(int value, vector<int> &indexes) {
    this->value = value;
    this->indexes = indexes;
  }
};

class Hungarian {
  int INF = INT32_MAX;
  vector<vector<int>> matrix;

public:
  explicit Hungarian(vector<vector<int>> &matrix) {
    this->matrix = vector<vector<int>>();
    this->matrix.emplace_back(vector<int>(matrix.size() + 1, 0));
    for (vector<int> &line : matrix) {
      this->matrix.emplace_back(vector<int>(1, 0));
      for(auto &i : line) {
        this->matrix.back().push_back(i);
      }
    }
  }

  AnswerPair solve() {
    vector<int> rows(matrix.size(), 0);
    vector<int> cols(matrix.size(), 0);
    vector<int> matching(matrix.size(), 0);
    vector<int> way(matrix.size(), 0);

    for (int row = 1; row < matrix.size(); row++) {
      matching[0] = row;
      vector<int> minValues(matrix.size(), INF);
      vector<bool> used(matrix.size(), false);
      int collumn = 0;
      do {
        used[collumn] = true;
        int curCol = 0;
        int delta = INF;
        int curRow = matching[collumn];

        for (int i = 1; i < matrix.size(); i++) {
          if (!used[i]) {
            int curDelta = matrix[curRow][i] - rows[curRow] - cols[i];
            if (curDelta < minValues[i]) {
              minValues[i] = curDelta;
              way[i] = collumn;
            }
            if (minValues[i] < delta) {
              delta = minValues[i];
              curCol = i;
            }
          }
        }
        for (int j = 0; j < matrix.size(); j++) {
          if (used[j]) {
            rows[matching[j]] += delta;
            cols[j] -= delta;
          } else {
            minValues[j] -= delta;
          }
        }
        collumn = curCol;

      } while (matching[collumn] != 0);

      do {
        int prev = way[collumn];
        matching[collumn] = matching[prev];
        collumn = prev;
      } while (collumn > 0);
    }
    vector<int> result(matrix.size() - 1, 0);
    for (int j = 1; j < matrix.size(); j++) {
      result[matching[j] - 1] = j;
    }
    return AnswerPair(-cols[0], result);
  }
};

int main() {
  int n;
  cin >> n;

  vector<vector<int>> matrix;
  for (int i = 0; i < n; ++i) {
    vector<int> temp;
    for (int j = 0, a; j < n; ++j) {
      cin >> a;
      temp.push_back(a);
    }
    matrix.push_back(temp);
  }

  cout << Hungarian(matrix).solve().to_str();
}
