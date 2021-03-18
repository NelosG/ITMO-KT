#include <algorithm>
#include <iostream>
#include <map>
#include <vector>
#include <sstream>

using namespace std;
void dfs_Right(int v, bool pick, vector<bool> &used_Left, vector<bool> &used_Right,
               vector<vector<int>> &left, vector<vector<int>> &right, vector<pair<int, int>> &edges);



bool dfs(int v, vector<vector<int>> &g, vector<bool> &used, vector<int> &matching) {
    if (!used[v]) {
        used[v] = true;
        for (int to : g[v]) {
            if (matching[to] == -1 || dfs(matching[to], g, used, matching)) {
                matching[to] = v;
                return true;
            }
        }
    }
    return false;
}


void dfs_Left(int v, bool pick, vector<bool> &used_Left, vector<bool> &used_Right,
              vector<vector<int>> &left, vector<vector<int>> &right, vector<pair<int, int>> &edges) {
    used_Left[v] = true;
    for (int i : left[v])
        if (i != -1 && !used_Right[i]) {
            if (pick) {
                edges.emplace_back(v, i);
            }
            dfs_Right(i,!pick, used_Left, used_Right, left, right, edges);
        }
}

void dfs_Right(int v, bool pick, vector<bool> &used_Left, vector<bool> &used_Right,
               vector<vector<int>> &left, vector<vector<int>> &right, vector<pair<int, int>> &edges) {
    used_Right[v] = true;
    for (int i : right[v])
        if (i != -1 && !used_Left[i]) {
            if (pick) {
                edges.emplace_back(i, v);
            }
            dfs_Left(i, !pick, used_Left, used_Right, left, right, edges);
        }
}

int main() {
    std::ios_base::sync_with_stdio(false);
    std::cin.tie(nullptr);
    std::cout.tie(nullptr);

    int n, m, e;
    cin >> n >> m >> e;

    vector<pair<int, int>> left_vertexes(n);
    vector<pair<int, int>> right_vertexes(m);


    for (int i = 0; i < left_vertexes.size(); ++i) {
        cin >> left_vertexes[i].first;
        left_vertexes[i].second = i;
    }
    for (int i = 0; i < right_vertexes.size(); ++i) {
        cin >> right_vertexes[i].first;
        right_vertexes[i].second = i;
    }


    map<pair<int, int>, int> indexes;
    vector<pair<int, int>> edges;
    vector<vector<int>> leftPart(n);
    vector<vector<int>> rightPart(m);
    for (int i = 0, from, to; i < e; ++i) {
        cin >> from >> to;
        --from;
        --to;
        indexes[{from, to}] = i + 1;
        leftPart[from].push_back(to);
        rightPart[to].push_back(from);
    }

    vector<pair<int, int>> sorted_Left = left_vertexes;
    vector<pair<int, int>> sorted_Right = right_vertexes;

    auto comp = [](pair<int, int> a, pair<int, int> b) { return a.first > b.first; };
    sort(sorted_Left.begin(), sorted_Left.end(),comp);
    sort(sorted_Right.begin(), sorted_Right.end(),comp);

    vector<int> left_Matching(n, -1);
    vector<int> right_Matching(m, -1);

    vector<bool> used_left(n, false);
    vector<bool> used_right(m, false);

    for (auto [w, i] : sorted_Left) {
        used_left.assign(n, false);
        dfs(i, leftPart, used_left, right_Matching);
    }

    for (auto [w, i] : sorted_Right) {
        used_right.assign(m, false);
        dfs(i, rightPart, used_right, left_Matching);
    }


    vector<vector<int>> left_Part(n);
    vector<vector<int>> right_Part(m);

    for (int i = 0; i < n; ++i) {
        if (left_Matching[i] != -1) {
            left_Part[i].push_back(left_Matching[i]);
            right_Part[left_Matching[i]].push_back(i);
        }
    }

    for (int i = 0; i < m; ++i) {
        if (right_Matching[i] != -1) {
            right_Part[i].push_back(right_Matching[i]);
            left_Part[right_Matching[i]].push_back(i);
        }
    }


    used_left.assign(n, false);
    used_right.assign(m, false);


    for (auto [w, i] : sorted_Left)
        if (left_Part[i].size() == 1 && !used_left[i]) {
            dfs_Left(i,true, used_left, used_right, left_Part, right_Part, edges);
        }

    for (auto [w, i] : sorted_Right)
        if (right_Part[i].size() == 1 && !used_right[i])
            dfs_Right(i,true, used_left, used_right, left_Part, right_Part, edges);

    for (int i = 0; i < n; ++i)
        if (!left_Part[i].empty() && !used_left[i])
            dfs_Left(i,true, used_left, used_right, left_Part, right_Part, edges);

    for (int i = 0; i < m; ++i)
        if (!right_Part[i].empty() && !used_right[i])
            dfs_Right(i,true, used_left, used_right, left_Part, right_Part, edges);

    stringstream out;
    long long int res = 0;
    for (auto edge : edges){
        res += left_vertexes[edge.first].first + right_vertexes[edge.second].first;
        out << indexes[edge] << ' ';
    }

    cout << res << '\n' << edges.size() << '\n' << out.str();
}
