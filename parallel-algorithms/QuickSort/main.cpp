#include <vector>
#include <iostream>
#include <random>
#include <chrono>
#include "quick_sort.h"
#include <algorithm>
#include <omp.h>

long long int run(std::vector<int> array, void (* sort)(std::vector<int>&), bool check);

void check_correctness(const std::vector<int>& array);

namespace chrono_time = std::chrono;

int main() {
    int count_of_runs = 5;
    int array_size = 1e8;
    bool check_correctness = true;
    std::vector<int> source(array_size);
    std::mt19937 gen(clock());

    omp_set_dynamic(true);
    omp_set_nested(true);

    std::vector<std::string> names;
    std::vector<void (*)(std::vector<int>&)> functions;

//    names.emplace_back("std::sort");
//    functions.push_back(standard::sort);
//
//    names.emplace_back("std::qsort");
//    functions.push_back(standard::qsort);
//
//    names.emplace_back("sequintal");
//    functions.push_back(sequintal::qsort);
//
    names.emplace_back("parallel");
    functions.push_back(parallel::qsort);

    names.emplace_back("parallel_v2");
    functions.push_back(parallel_v2::qsort);

    std::vector<std::vector<long long>> times(functions.size(), std::vector<long long>());

    for (int i = 0; i < count_of_runs; ++i) {

        generate(source.begin(), source.end(), gen);
        for (int j = 0; j < functions.size(); ++j) {
            times[j].push_back(
                    run(source, functions[j], check_correctness)
            );
        }
    }
    for (int i = 0; i < names.size(); ++i) {
        long long average = 0;
        for (auto& time: times[i]) {
            average += time;
        }
        average /= count_of_runs;

        std::cout << "Average " << names[i] << " " << average << "ms\n";
    }

    return 0;
}

long long run(std::vector<int> array, void (* sort)(std::vector<int>&), bool check) {
    chrono_time::steady_clock::time_point start, end;
    start = chrono_time::steady_clock::now();
    sort(array);
    end = chrono_time::steady_clock::now();
    if (check) {
        check_correctness(array);
    }

    return chrono_time::duration_cast<chrono_time::milliseconds>(end - start)
            .count();
}

void check_correctness(const std::vector<int>& array) {
    for (int i = 1; i < array.size(); ++i) {
        if (array[i - 1] > array[i]) {
            exit(11);
        }
    }
}
