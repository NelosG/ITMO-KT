#include <vector>
#include <iostream>
#include <random>
#include <chrono>
#include <algorithm>
#include <omp.h>
#include "scan.h"

long long run(const std::vector<long long>& vec, std::vector<long long>& result,
              void (* scan)(const std::vector<long long>&, std::vector<long long>&),
              bool check_correctness);

namespace chrono_time = std::chrono;


int main() {
    int count_of_runs = 5;
    int array_size = 1e8;
    bool check_correctness = true;

    std::vector<long long> vec(array_size);
    std::vector<long long> result(vec.size());
    std::mt19937_64 gen(clock());

    std::vector<std::string> names;
    std::vector<void (*)(const std::vector<long long>&, std::vector<long long>&)> functions;

    omp_set_dynamic(true);
    omp_set_nested(true);
    omp_set_num_threads(omp_get_thread_limit());

//    names.emplace_back("sequintal");
//    functions.push_back(sequintal::scan);

//    names.emplace_back("parallel");
//    functions.push_back(parallel::scan);

    names.emplace_back("sequintal_v2");
    functions.push_back(sequintal_v2::scan);

    names.emplace_back("parallel_v2");
    functions.push_back(parallel_v2::scan);

    std::vector<std::vector<long long>> times(functions.size(), std::vector<long long>());



    for (int i = 0; i < count_of_runs; ++i) {

        generate(vec.begin(), vec.end(), gen);
        for (int j = 0; j < functions.size(); ++j) {
            times[j].push_back(
                    run(vec, result, functions[j], check_correctness)
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

long long run(const std::vector<long long>& vec, std::vector<long long>& result,
              void (* scan)(const std::vector<long long>&, std::vector<long long>&),
              bool check_correctness) {
    chrono_time::steady_clock::time_point start, end;

    start = chrono_time::steady_clock::now();
    scan(vec, result);
    end = chrono_time::steady_clock::now();


    if (check_correctness) {
        long long counter = 0L;

        for (int ind = 0; ind < vec.size(); ++ind) {
            counter += vec[ind];
            if (counter != result[ind]) {
                exit(11);
            }
        }
    }
    return chrono_time::duration_cast<chrono_time::milliseconds>(end - start).count();
}
