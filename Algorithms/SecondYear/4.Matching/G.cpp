#include <iostream>
#include <vector>

using namespace std;

vector<bool> visited;
vector<int> matching;
int greenCount, yellowCount, countDoubleAquarium, countPair, countNotSingle;
vector<vector<int>> matrix;


bool dfs(int v) {
    if (!visited[v]) {
        visited[v] = true;
        for (int i = 0; i < matrix.size(); i++)
            if (matrix[v][i] != 0)
                if (matching[i] == -1 || dfs(matching[i])) {
                    matching[i] = v;
                    return true;
                }
    }
    return false;
}

void read() {
    cin >> greenCount >> yellowCount >> countDoubleAquarium;
    matrix.assign(greenCount + yellowCount - countDoubleAquarium,
                  vector<int>(greenCount + yellowCount - countDoubleAquarium, 1));
    for (int i = greenCount; i < matrix.size(); i++) {
        fill(matrix[i].begin() + yellowCount, matrix[i].end(), 0);
    }

    cin >> countPair;
    for (int i = 0; i < countPair; i++) {
        int x, y;
        cin >> x >> y;
        --x, --y;
        if (x > y)
            swap(x, y);
        matrix[x][y - greenCount] = 0;
    }

    cin >> countNotSingle;
    for (int i = 0, x; i < countNotSingle; i++) {
        cin >> x;
        --x;
        if (x < greenCount) {
            fill(matrix[x].begin() + yellowCount, matrix[x].end(), 0);
        } else {
            for (int j = greenCount; j < matrix.size(); j++) {
                matrix[j][x - greenCount] = 0;
            }
        }
    }
}

int main() {
    read();
    matching.assign(matrix.size(), -1);
    visited.assign(matrix.size(), false);
    for (int i = 0; i < matrix.size(); i++) {
        fill(visited.begin(), visited.end(), false);
        if (!dfs(i)) {
            cout << "NO\n";
            return 0;
        }
    }
    cout << "YES\n";
    for (int i = 0; i < yellowCount; i++)
        if (matching[i] < greenCount)
            cout << matching[i] + 1 << " " << (i + greenCount + 1) << '\n';
}
