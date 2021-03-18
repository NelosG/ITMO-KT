#include <iostream>
#include <queue>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

using namespace std;

struct node {
    char ch;
    int parent;
    unordered_map<char, int> childs;
    int link = -1;
    bool is_term = false;
    string word;

    node(char c, int par) : ch(c), parent(par){};
};

int const root = 0;
vector<node> nodes;

void add_str(string &&s) {
    int current = root;
    int ptr = 0;
    while (nodes[current].childs.count(s[ptr]))
        current = nodes[current].childs[s[ptr++]];
    for (int i = ptr; i < s.size(); i++) {
        int j = (int) nodes.size();
        nodes[current].childs[s[i]] = j;
        nodes.emplace_back(s[i], current);
        current = j;
    }
    if (current != root) {
        nodes[current].is_term = true;
        nodes[current].word = std::move(s);
    }
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


int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int n;
    cin >> n;

    nodes.emplace_back('\0', -1);

    vector<string> words;
    unordered_map<string, int> answer;

    for (int i = 0; i < n; i++) {
        string s;
        cin >> s;
        words.push_back(s);
        answer[s] = 0;
        add_str(std::move(s));
    }

    build_suf_links();

    string text;
    cin >> text;

    unordered_map<int, int> states;

    int cur = 0;
    for (char c : text) {
        if (nodes[cur].childs.count(c)) {
            cur = nodes[cur].childs[c];
        } else if (cur != root) {
            while (nodes[cur].link != -1 && !nodes[cur].childs.count(c))
                cur = nodes[cur].link;
            cur = nodes[cur].childs.count(c) ? nodes[cur].childs[c] : root;
        }
        if (cur != root) {
            if (states.count(cur)) states[cur]++;
            else
                states[cur] = 1;
        }
    }


    for (auto [st, count] : states) {
        int p = st;
        while (true) {
            if (nodes[p].is_term) {
                answer[nodes[p].word] += count;
            }
            p = nodes[p].link;
            if (p == -1) break;
        }
    }

    for (const auto &word : words)
        cout << (answer[word]) << '\n';
}
