#define _CRT_DISABLE_PERFCRIT_LOCKS
#define _USE_MATH_DEFINES

#include <iostream>
#include <string>
#include <vector>
#include <cmath>
#include <algorithm>
#include <map>
#include <set>
#include <iomanip>

using namespace std;

class NaiveBayes {
    int k;
    double alpha;
    vector<double> lambdas;
    vector<map<string, int>> counts;
    vector<int> classCounts;
    set<string> allWords;
    vector<map<string, double>> problem;
    int n;
public:
    NaiveBayes(int k, double alpha, vector<double> &lambdas) : k(k), alpha(alpha), lambdas(lambdas) {
        counts.assign(k, map<string, int>());
        classCounts.assign(k, 0);
        n = 0;
    }

    void fit(int c, const set<string> &words) {
        for (const string &word: words) {
            allWords.insert(word);
        }
        for (const string &word: words) {
            counts[c - 1][word] += 1;
        }
        classCounts[c - 1] += 1;
        n++;
    }

    void readyToPredict() {
        problem.clear();
        problem.assign(k, map<string, double>());
        for (int i = 0 ; i < k ; i++) {
            for (auto &entry: counts[i]) {
                problem[i][entry.first] = ((double)entry.second + alpha) / (classCounts[i] + 2 * alpha);
            }
        }
    }


    vector<double> predict(const set<string> &words) {
        vector<double> scores;
        for (int j = 0 ; j < k ; j++) {
            scores.emplace_back(0.0);
        }
        int counter = 0;
        double sum = 0;
        for (int j = 0 ; j < k ; j++) {
            if (classCounts[j] != 0) {
                double score = log((double)lambdas[j] * classCounts[j] / n);
                for (const string &word: allWords) {
                    double v = (alpha / ((double)classCounts[j] + alpha * 2));
                    if (problem[j].count(word)) {
                        v = problem[j][word];
                    }
                    if (words.count(word)) {
                        score += log(v);
                    } else {
                        score += log(1 - v);
                    }
                }
                scores[j] = score;
                counter++;
                sum += score;
            }
        }
        double avg = -sum / counter;
        sum = 0;
        for (int j = 0 ; j < k ; j++) {
            if (classCounts[j] != 0) {
                scores[j] = exp(avg + scores[j]);
                sum += scores[j];
            }
        }
        for (int j = 0 ; j < k ; j++) {
            scores[j] = scores[j] / sum;
        }
        return scores;
    }
};

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);
    cout << setprecision(8);

    int k;
    cin >> k;
    vector<double> lambdas;
    for (int i = 0, temp ; i < k ; i++) {
        lambdas.emplace_back((cin >> temp, temp));
    }

    int alpha, n;
    cin >> alpha >> n;

    NaiveBayes naiveBayes(k, alpha, lambdas);
    for (int i = 0, c, counter ; i < n ; i++) {
        cin >> c >> counter;
        set<string> words;
        for (int j = 0 ; j < counter ; j++) {
            string s;
            cin >> s;
            words.emplace(s);
        }
        naiveBayes.fit(c, words);
    }

    naiveBayes.readyToPredict();

    int m;
    cin >> m;

    for (int i = 0, count ; i < m ; i++) {
        cin >> count;
        set<string> words;
        for (int j = 0 ; j < count ; j++) {
            string s;
            cin >> s;
            words.emplace(s);
        }

        for (auto &score: naiveBayes.predict(words)) {
            cout << score << ' ';
        }
        cout << '\n';
    }
    cout.flush();
    return 0;
}
