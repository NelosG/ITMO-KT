#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;

class vert_section {
public:
    int x;
    int y1, y2;
    vert_section(int x, int y1, int y2) {
        this->x = x;
        this->y1 = y1;
        this->y2 = y2;
    }
};

class horiz_section {
public:
    int x1, x2;
    int y;
    horiz_section(int x1, int x2, int y) {
        this->x1 = x1;
        this->x2 = x2;
        this->y = y;
    }
};

bool check(horiz_section &a, vert_section & b){
    if(b.y1 <= a.y && a.y <= b.y2)
        if(a.x1 <= b.x && b.x <= a.x2)
            return true;
    return false;
}

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

int main(){
    int n;
    cin >> n;
    vector<horiz_section> horizVec;
    vector<vert_section> vertVec;
    for(int i = 0, a, b, c, d; i < n; ++i) {
        cin >> a >> b >> c >> d;
        if(a > c) swap(a, c);
        if(b > d) swap (b, d);
        if(a == c) {
            vertVec.emplace_back(a, b, d);
        } else {
            horizVec.emplace_back(a, c, d);
        }
    }
    vector<vector<int>> g(horizVec.size(), vector<int>());

    for(int i = 0; i < horizVec.size(); i++) {
        for (int j = 0; j < vertVec.size(); j++) {
            if(check(horizVec[i], vertVec[j])){
                g[i].push_back(j);
            }
        }
    }




    vector<bool> used_horiz(horizVec.size(), false);
    vector<int> matching(vertVec.size(), -1);

    for (int i = 0; i < horizVec.size(); i++) {
        used_horiz.assign(horizVec.size(), false);
        dfs(i, g, used_horiz, matching);
    }

    vector<bool> horiz_mask(horizVec.size(), false);
    for (int a : matching)
        if (a != -1)
            horiz_mask[a] = true;

    used_horiz.assign(horizVec.size(), false);
    vector<bool> used_vert(vertVec.size(), false);

    for (int i = 0; i < horizVec.size(); i++)
        if (!horiz_mask[i])
            dfs2(i, g, used_horiz, used_vert, matching);

    int horiz_count = 0;
    int vert_count = 0;


    for (int i = 0; i < max(horizVec.size(), vertVec.size()); ++i) {
        if (i < horizVec.size() && used_horiz[i]) {
            horiz_count++;
        }
        if (i < vertVec.size() && !used_vert[i]) {
            vert_count++;
        }
    }
    cout << horiz_count + vert_count;
}
