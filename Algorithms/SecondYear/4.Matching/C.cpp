#include <algorithm>
#include <iostream>
#include <sstream>
#include <vector>

using namespace std;

bool dfs(int u, vector<vector<int>> &g, vector<bool> &used, vector<int> &matching) {
    if (used[u]) return false;
    used[u] = true;
    for (int j : g[u]) {
        if (matching[j] == -1 || dfs(matching[j], g, used, matching)) {
            matching[j] = u;
            return true;
        }
    }
    return false;
}

void dfs2(int u, vector<vector<int>> &g, vector<bool> &used_boys, vector<bool> &used_girls, vector<int> &matching) {
    if (used_boys[u]) return;
    used_boys[u] = true;

    for (int j : g[u]) {
        if (!used_girls[j]) {
            used_girls[j] = true;
            dfs2(matching[j], g, used_boys, used_girls, matching);
        }
    }
}


int main() {
    int k;
    cin >> k;
    for (int iter = 0; iter < k; iter++) {
        int left;
        int right;
        cin >> left >> right;
        vector<vector<int>> g(left, vector<int>());

        for (int i = 0, a; i < left; i++) {
            vector<bool> to_girls_mask(right, false);
            while ((cin >> a, a) != 0)
                to_girls_mask[a - 1] = true;

            for (int j = 0; j < right; j++)
                if (!to_girls_mask[j])
                    g[i].push_back(j);
        }



        vector<bool> used_boys(left, false);
        vector<int> matching(right, -1);
        for (int i = 0; i < left; i++) {
            used_boys.assign(left, false);
            dfs(i, g, used_boys, matching);
        }

        vector<bool> boys_mask(left);
        for (int a : matching)
            if (a != -1)
                boys_mask[a] = true;

        used_boys.assign(left, false);
        vector<bool> used_girls(right, false);
        for (int i = 0; i < left; i++)
            if (!boys_mask[i])
                dfs2(i, g, used_boys, used_girls, matching);

        int boys_count = 0;
        int girls_count = 0;


        stringstream boys;
        stringstream girls;
        for (int i = 1; i <= max(left, right); ++i) {
            if (i <= left && used_boys[i - 1]) {
                boys_count++;
                boys << i << " ";
            }
            if (i <= right && !used_girls[i - 1]) {
                girls << i << " ";
                girls_count++;
            }
        }
        cout << boys_count + girls_count << '\n'
             << boys_count << " " << girls_count << '\n'
             << boys.str() << '\n'
             << girls.str() << "\n\n";
    }
}
