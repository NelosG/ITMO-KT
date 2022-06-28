#include <iostream>
#include <string>
#include <vector>
#include <cmath>
#include <algorithm>
#include <map>
#include <set>
#include <sstream>
#include <optional>

using namespace std;

class ResultPair {

public:
    ResultPair(int divider, double hold) : divider(divider), hold(hold) {
    }

    int divider;
    double hold;
};

optional<ResultPair> split(int k, vector<pair<vector<int>, int>> &beans) {
    double bestScore = -1e20;
    optional<ResultPair> bestSplit;
    for (int i = 0 ; i < beans[0].first.size() ; i++) {
        sort(beans.begin(), beans.end(),
             [i](auto o1, auto o2) { return (o2.first[i] - o1.first[i]) < 0; });
        if (beans[0].first[i] == beans[beans.size() - 1].first[i]) {
            continue;
        }
        vector<int> left(k + 1, 0);
        vector<int> right(k + 1, 0);
        int leftSize = 0;
        size_t rightSize = beans.size();
        long long leftSum = 0;
        long long rightSum = 0;
        for (auto &bean: beans) {
            rightSum -= right[bean.second] * right[bean.second];
            right[bean.second] += 1;
            rightSum += right[bean.second] * right[bean.second];
        }
        int previous = -1;
        for (int j = 0 ; j < beans.size() ; j++) {
            if (j != 0 && beans[j].first[i] != previous) {
                double score = 1.0 * leftSum / leftSize + 1.0 * rightSum / rightSize;
                if (score > bestScore) {
                    bestScore = score;
                    bestSplit.emplace(ResultPair(i, ((double)previous + beans[j].first[i]) / 2));
                }
            }
            rightSum -= right[beans[j].second] * right[beans[j].second];
            right[beans[j].second] -= 1;
            rightSum += right[beans[j].second] * right[beans[j].second];
            rightSize--;

            leftSum -= left[beans[j].second] * left[beans[j].second];
            left[beans[j].second] += 1;
            leftSum += left[beans[j].second] * left[beans[j].second];
            leftSize++;

            previous = beans[j].first[i];
        }
    }
    return bestSplit;
}

class Node {

protected:
    int id;

    explicit Node(int id) : id(id) {}

public:
    [[nodiscard]] int get_id() const {
        return id;
    }

    virtual void to_string(stringstream& ss) = 0;
};

class Leaf : public Node {
    int group;
public:
    Leaf(int group, int id) : Node(id), group(group), Node(TensorSize(0, 0, 0)) {
    }

    [[nodiscard]] int get_group() const {
        return group;
    }

    void to_string(stringstream& ss) override {
        ss << "C " << group << '\n';
    }
};

class EntireNode : public Node {
    int divider;
    double hold;
    Node *left;
    Node *right;
public:
    EntireNode(Node *left, Node *right, int divider, double hold, int id = 0) : Node(id),
                                                                                left(left),
                                                                                right(right),
                                                                                divider(divider),
                                                                                hold(hold), Node(TensorSize(0, 0, 0)) {

    }

    void to_string(stringstream& ss) override {
        ss << "Q " << divider + 1 << ' ' << hold << ' ' << left->get_id() << ' ' << right->get_id()
           << '\n';
        left->to_string(ss);
        right->to_string(ss);
    }

};

class decision_tree {
    int k;
    Node *root;
    int id;
    int max_depth;

    [[nodiscard]] int dominantClass(const vector<pair<vector<int>, int>> &beans) const {
        vector<int> counts(k + 1, 0);
        for (auto &bean: beans) {
            counts[bean.second] += 1;
        }
        int max = *std::max_element(counts.begin(), counts.end());
        for (int i = 0 ; i < counts.size() ; i++) {
            if (counts[i] == max) {
                return i;
            }
        }
        return -1;
    }

    Node *train(vector<pair<vector<int>, int>> &beans, int depth) {
        int nodeId = ++id;
        if (depth == max_depth) {
            return new Leaf(dominantClass(beans), nodeId);
        }
        int minima = INT32_MAX;
        int maxima = INT32_MIN;
        for (auto &bean: beans) {
            minima = min(minima, bean.second);
            maxima = max(maxima, bean.second);
        }
        if (minima == maxima) {
            return new Leaf(beans[0].second, nodeId);
        }
        optional<ResultPair> splitt = split(k, beans);
        if (splitt.has_value()) {
            ResultPair result = splitt.value();

            vector<pair<vector<int>, int>> left;
            vector<pair<vector<int>, int>> right;
            for (auto &bean: beans) {
                int temp = bean.first[result.divider];
                if (temp < result.hold) {
                    left.emplace_back(bean);
                } else {
                    right.emplace_back(bean);
                }
            }
            return new EntireNode(train(left, depth + 1),
                                  train(right, depth + 1),
                                  result.divider, result.hold, nodeId);
        } else {
            return new Leaf(dominantClass(beans), nodeId);
        }
    }

public:
    decision_tree(int k, int max_depth) : root(new Leaf(-1, -1)) {
        this->k = k;
        this->max_depth = max_depth;
        this->id = 0;
    }

    void train(vector<pair<vector<int>, int>>& beans) {
        root = train(beans, 0);
    }

    string to_string() {
        stringstream ss;
        ss << id << '\n';
        root->to_string(ss);
        return ss.str();
    }
};

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int m;
    int k;
    int max_depth;
    int n;
    cin >> m >> k >> max_depth >> n;
    vector<pair<vector<int>, int>> beans;
    for (int i = 0, tmp ; i < n ; i++) {
        vector<int> numbers;
        for (int j = 0 ; j < m ; j++) {
            cin >> tmp;
            numbers.emplace_back(tmp);
        }
        cin >> tmp;
        beans.emplace_back(numbers, tmp);
    }
    if
    decision_tree tree(k, max_depth);
    tree.train(beans);
    cout << tree.to_string();
    cout.flush();
    return 0;
}
