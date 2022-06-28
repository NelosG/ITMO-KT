#define _CRT_DISABLE_PERFCRIT_LOCKS
#define _USE_MATH_DEFINES

#include <iostream>
#include <vector>
#include <cmath>
#include <algorithm>
#include <map>
#include <functional>
#include <iomanip>

using namespace std;

double eps = 1e-60;

bool zero(double a) {
    if (abs(a) < eps) {
        return true;
    }
    return false;
}


double uniform_K(double u) {
    return abs(u) < 1 ? 0.5 : 0;
}


double triangular_K(double u) {
    double abs_u = abs(u);
    return abs_u < 1 ? 1 - abs_u : 0;
}


double epanechnikov_K(double u) {
    return abs(u) < 1 ? 0.75 * (1 - u * u) : 0;
}


double quartic_K(double u) {
    return abs(u) < 1 ? 15. / 16. * pow(1 - u * u, 2) : 0;
}


double triweight_K(double u) {
    return abs(u) < 1 ? 35. / 32 * pow(1 - u * u, 3) : 0;
}


double tricube_K(double u) {
    return abs(u) < 1 ? 70. / 81 * pow(1 - pow(abs(u), 3), 3) : 0;
}


double gaussian_K(double u) {
    return (1. / sqrt(2 * M_PI)) * pow(M_E, (-1. / 2 * u * u));
}


double cosine_K(double u) {
    return abs(u) < 1 ? M_PI / 4 * cos(M_PI / 2 * u) : 0;
}


double logistic_K(double u) {
    return 1. / (pow(M_E, u) + pow(M_E, (-u)) + 2);
}


double sigmoid_K(double u) {
    return 2. / M_PI / (pow(M_E, u) + pow(M_E, (-u)));
}


double manhattan_distance(const vector<double> &row1, const vector<double> &row2) {
    double distance = 0.0;
    for (int i = 0 ; i < row2.size() - 1 ; ++i) {
        distance += abs(row1[i] - row2[i]);
    }
    return distance;
}

double euclidean_distance(const vector<double> &row1, const vector<double> &row2) {
    double distance = 0.0;
    for (int i = 0 ; i < row2.size() - 1 ; ++i) {
        distance += pow(row1[i] - row2[i], 2);
    }
    return sqrt(distance);
}

double chebyshev_distance(const vector<double> &row1, const vector<double> &row2) {
    double distance = 0.0;
    for (int i = 0 ; i < row2.size() - 1 ; ++i) {
        distance = max(distance, abs(row1[i] - row2[i]));
    }
    return distance;
}


vector<pair<vector<double>, double>> sort_dtrain(vector<vector<double>> &train,
                                                 vector<double> &test_row,
                                                 function<double(const vector<double> &,
                                                                 const vector<double> &)> &func_distance) {

    vector<pair<vector<double>, double>> distances;
    distances.reserve(train.size());
    for (const auto &train_row: train) {
        distances.emplace_back(train_row, func_distance(test_row, train_row));
    }
    sort(distances.begin(), distances.end(),
         [](pair<vector<double>, double> &dist1, pair<vector<double>, double> &dist2) {
             return dist1.second < dist2.second;
         });
    return distances;
}


map<string, function<double(const vector<double> &, const vector<double> &)>> metric_dict{
        {"manhattan", manhattan_distance},
        {"euclidean", euclidean_distance},
        {"chebyshev", chebyshev_distance},
};

map<string, function<double(double)>> K_dict{
        {"uniform",      uniform_K},
        {"triangular",   triangular_K},
        {"epanechnikov", epanechnikov_K},
        {"quartic",      quartic_K},
        {"triweight",    triweight_K},
        {"tricube",      tricube_K},
        {"gaussian",     gaussian_K},
        {"cosine",       cosine_K},
        {"logistic",     logistic_K},
        {"sigmoid",      sigmoid_K},
};

double get_zero_dist(const vector<pair<vector<double>, double>> &sorted_dataset) {
    int zero_count = 0;
    double zero_sum = 0;

    for (auto &x: sorted_dataset) {
        if (zero(x.second)) {
            zero_count += 1;
        }
    }
    for (auto &x: sorted_dataset) {
        if (zero(x.second)) {
            zero_sum += x.first[x.first.size() - 1];
        }
    }
    if (zero_count != 0) {
        return zero_sum / zero_count;
    } else {
        double res = 0;
        for (auto &x: sorted_dataset) {
            res += x.first[x.first.size() - 1];
        }
        return res / sorted_dataset.size();
    }
}

double find_a(const vector<pair<vector<double>, double>> &sorted_dataset,
              double window,
              const function<double(double)> &k_function) {
    double sum_y = 0;
    double sum = 0;
    if (zero(window)) {
        return get_zero_dist(sorted_dataset);
    }
    for (auto &pair: sorted_dataset) {
        double temp = k_function(pair.second / window);
        sum_y += pair.first[pair.first.size() - 1] * temp;
        sum += temp;
    }
    if (zero(sum)) {
        return get_zero_dist(sorted_dataset);
    }
    return sum_y / sum;
}

int main() {
    ios_base::sync_with_stdio(false);
    cin.tie(nullptr);
    cout.tie(nullptr);
    int n, m;
    cin >> n >> m;
    vector<vector<double>> dtest(n, vector<double>(m + 1, 0));
    vector<double> row(m + 1, 0);
    for (int i = 0 ; i < n ; ++i) {
        for (int j = 0 ; j <= m ; ++j) {
            cin >> dtest[i][j];
        }
    }
    for (int j = 0 ; j < m ; ++j) {
        cin >> row[j];
    }
    string distance, kernel, window_type;
    cin >> distance >> kernel >> window_type;
    auto metric = metric_dict[distance];
    auto k_type = K_dict[kernel];
    vector<pair<vector<double>, double>> sorted_dataset = sort_dtrain(dtest, row, metric);
    double window = 0;
    if (window_type == "variable") {
        int k;
        cin >> k;
        window = sorted_dataset[k].second;
    } else {
        cin >> window;
    }
    double res = find_a(sorted_dataset, window, k_type);

    cout << setprecision(7) << res;
    return 0;
}
