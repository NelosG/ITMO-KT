#include <iostream>
#include <queue>
#include <unordered_map>
#include <unordered_set>
#include <vector>
#include <string>

using namespace std;

struct node {
    char ch;
    int parent;
    unordered_map<char, int> childs;
    int link = -1;
    bool is_term = false;

    node(char c, int par) : ch(c), parent(par){};
};

int const root = 0;
vector<node> nodes;

void add_str(string const & s) {
    int current = root;
    int ptr = 0;
    while (nodes[current].childs.count(s[ptr]))
        current = nodes[current].childs[s[ptr++]];
    for (int i = ptr; i < s.size(); i++) {
        int j = (int)nodes.size();
        nodes[current].childs[s[i]] = j;
        nodes.emplace_back(s[i], current);
        current = j;
    }
    if (current != root) nodes[current].is_term = true;
}

void build_suf_links() {
    queue<int> q;
    for (auto [c, v] : nodes[root].childs)
        q.push(v);
    while (!q.empty()) {
        int v = q.front();
        q.pop();
        int p = nodes[nodes[v].parent].link;
        while (p != -1 && !nodes[p].childs.count(nodes[v].ch))
            p = nodes[p].link;
        nodes[v].link = p == -1 ? root : nodes[p].childs[nodes[v].ch];
        for (auto [c, i] : nodes[v].childs)
            q.push(i);
    }
}

string get_word(int cur) {
    string s;
    while (nodes[cur].parent != -1) {
        s = nodes[cur].ch + s;
        cur = nodes[cur].parent;
    }
    return s;
}

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int n;
    cin >> n;

    nodes.emplace_back('\0', -1);

    vector<string> words;

    for (int i = 0; i < n; i++) {
        string s;
        cin >> s;
        add_str(s);
        words.push_back(s);
    }

    build_suf_links();

    string text;
    cin >> text;

    unordered_set<int> states;

    int cur = 0;
    for (char c : text) {
        if (nodes[cur].childs.count(c)) {
            cur = nodes[cur].childs[c];
        } else if (cur != root) {
            while (nodes[cur].link != -1 && !nodes[cur].childs.count(c))
                cur = nodes[cur].link;
            cur = nodes[cur].childs.count(c) ? nodes[cur].childs[c] : root;
        }
        if (cur != root) states.insert(cur);
    }


    unordered_set<string> answer;
    for (int p : states) {
        while (true) {
            if (nodes[p].is_term) answer.insert(get_word(p));
            p = nodes[p].link;
            if (p == -1 || states.count(p)) break;
        }
    }

    for (const auto &word : words)
        cout << (answer.count(word) ? "YES" : "NO") << '\n';
}
