#define _CRT_DISABLE_PERFCRIT_LOCKS

#include <iostream>
#include <vector>
#include <iomanip>

using namespace std;

class answer {

    vector<int> TPs; // true positive
    vector<int> FPs; // false positive
    vector<int> FNs; // false negative

    vector<double> F_scores;
    vector<double> recalls;
    vector<double> precisions;

    int total_count;
    vector<int> counts_classes;


    void calc_all(vector<vector<int>> &conf_matrix) {
        for (int i = 0 ; i < conf_matrix.size() ; ++i) {
            for (int j = 0 ; j < conf_matrix.size() ; ++j) {
                if (i == j) {
                    TPs[i] += conf_matrix[i][i];
                } else {
                    FPs[i] += conf_matrix[j][i];
                    FNs[i] += conf_matrix[i][j];
                }
                total_count += conf_matrix[i][j];
                counts_classes[i] += conf_matrix[i][j];
            }
        }

        for (int i = 0 ; i < conf_matrix.size() ; ++i) {
            for (int j = 0 ; j < conf_matrix.size() ; ++j) {
                recalls[i] = (TPs[i] + FNs[i]) != 0 ?
                             (double)TPs[i] / (TPs[i] + FNs[i]) :
                             0;

                precisions[i] = (TPs[i] + FPs[i]) != 0 ?
                                (double)TPs[i] / (TPs[i] + FPs[i]) :
                                0;

                F_scores[i] = (precisions[i] + recalls[i]) != 0 ?
                              2 * ((precisions[i] * recalls[i]) / (precisions[i] + recalls[i])) :
                              0;
            }
        }
        calc_micro();
        calc_macro();
        calc_f_score();
    }

    void calc_micro() {
        double TP = 0, FP = 0, FN = 0, recall, precision;
        for (int i = 0 ; i < TPs.size() ; ++i) {
            TP += TPs[i] * counts_classes[i];
            FP += FPs[i] * counts_classes[i];
            FN += FNs[i] * counts_classes[i];
        }
        TP /= total_count;
        FP /= total_count;
        FN /= total_count;
        precision = TP / (TP + FP);
        recall = TP / (TP + FN);
        micro_f = (precision + recall) != 0 ?
                  2 * ((precision * recall) / (precision + recall)) :
                  0;
    }

    void calc_macro() {
        double recall = 0, precision = 0;
        for (int i = 0 ; i < recalls.size() ; ++i) {
            recall += recalls[i] * counts_classes[i];
            precision += precisions[i] * counts_classes[i];
        }
        recall /= total_count;
        precision /= total_count;

        macro_f = (precision + recall) != 0 ?
                  2 * ((precision * recall) / (precision + recall)) :
                  0;
    }

    void calc_f_score() {
        f_score = 0;
        for (int i = 0 ; i < F_scores.size() ; ++i) {
            f_score += F_scores[i] * counts_classes[i];
        }
        f_score /= total_count;
    }

public:

    explicit answer(vector<vector<int>> &conf_matrix) {
        TPs.assign(conf_matrix.size(), 0);
        FPs.assign(conf_matrix.size(), 0);
        FNs.assign(conf_matrix.size(), 0);
        recalls.assign(conf_matrix.size(), 0);
        precisions.assign(conf_matrix.size(), 0);
        counts_classes.assign(conf_matrix.size(), 0);
        F_scores.assign(conf_matrix.size(), 0);
        total_count = 0;


        calc_all(conf_matrix);
    }

    double f_score = 0;
    double micro_f = 0;
    double macro_f = 0;
};


int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);

    int n;
    cin >> n;

    vector<vector<int>> conf_matrix(n, vector<int>(n, 0));
    for (int i = 0 ; i < n ; ++i) {
        for (int j = 0 ; j < n ; ++j) {
            cin >> conf_matrix[i][j];
        }
    }
    answer ans(conf_matrix);
    cout << setprecision(7) << ans.micro_f << '\n' << ans.macro_f << '\n' << ans.f_score;
    return 0;
}
