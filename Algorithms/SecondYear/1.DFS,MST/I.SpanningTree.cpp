#include <vector>
#include <iostream>

static int INF = 2000000000;
using namespace std;

static double distance(int x, int y, const vector<pair<int, int>> &graph) {
    if (x == y)
        return INF;
    pair<int, int> a = graph[x];
    pair<int, int> b = graph[y];
    return sqrt((b.first - a.first) * (b.first - a.first) +
                (b.second - a.second) * (b.second - a.second));
}

int main() {
    int n;
    vector<pair<int, int>> graph;
    vector<int> selectedRib;
    vector<double> minRib;
    cin >> n;
    vector<bool> used(n, 0);

    for (int i = 0; i < n; i++) {
        selectedRib.push_back(-1);
        minRib.push_back((double) INF);
    }
    minRib[0] = 0.0;

    for (int i = 0; i < n; ++i) {
        int from, to;
        cin >> from >> to;
        graph.emplace_back(from, to);
    }

    double answer = 0;
    for (int i = 0; i < n; i++) {
        int v = -1;
        for (int j = 0; j < n; j++)
            if (v == -1 || minRib[v] > minRib[j])
                if (!used[j])
                    v = j;

        used[v] = true;
        if (selectedRib[v] != -1)
            answer += distance(v, selectedRib[v], graph);

        for (int j = 0; j < n; j++) {
            double dist = distance(v, j, graph);
            if (minRib[j] > dist) {
                minRib[j] = dist;
                selectedRib[j] = v;
            }
        }
    }
    cout << answer;
}
