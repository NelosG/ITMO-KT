#include <iostream>
#include <list>
#include <vector>
#include "graph/graph.h"
#include "bfs.h"
#include <chrono>
#include <functional>
#include <omp.h>
#include <climits>

namespace chrono_time = std::chrono;

void verify(const std::vector<int>& expected, const std::vector<int>& actual, graph& graph) {
    if (expected.size() != actual.size()) {
        std::cerr << "Different sizes\n";
        exit(666);
    }
    if (!std::equal(expected.begin(), expected.end(), actual.begin())) {
        std::cerr << "Not equal\n";
        exit(666);
    }
//    for (int i = 0; i < actual.size(); ++i) {
//        int x1, x2, x3;
//        graph.use_mask(i, x1, x2, x3);
//
//        if (!graph.validUIndex(x1)
//            || !graph.validUIndex(x2)
//            || !graph.validUIndex(x3)) {
//            continue;
//        }
//
//        if (actual[i] != x1 + x2 + x3) {
//            std::cerr << "Incorrect distance: D: " << actual[i] << ", ";
//            std::cerr << "Xs: " << x1 << " " << x2 << " " << x3 << ",";
//            std::cerr << "XsSum: " << (x1 + x2 + x3) << "\n";
//            exit(666);
//        }
//
//        int p1 = x1 > 0 ? actual[graph.toUId(x1 - 1, x2, x3)] : INT_MIN;
//        int p2 = x2 > 0 ? actual[graph.toUId(x1, x2 - 1, x3)] : INT_MIN;
//        int p3 = x3 > 0 ? actual[graph.toUId(x1, x2, x3 - 1)] : INT_MIN;
//
//        if (x1 + x2 + x3 != 0) {
//
//            if (p1 != actual[i] - 1 && p2 != actual[i] - 1 && p3 != actual[i] - 1) {
//                std::cerr << "Incorrect distance: D: " << actual[i] << ", ";
//                std::cerr << "Xs: " << x1 << " " << x2 << " " << x3 << ",";
//                std::cerr << "XsSum: " << (x1 + x2 + x3) << "\n";
//                std::cerr << "\t" << p1 << " " << p2 << " " << p3 << ",";
//                exit(666);
//            }
//
//            if (p1 == INT_MIN) p1 = INT_MAX;
//            if (p2 == INT_MIN) p2 = INT_MAX;
//            if (p3 == INT_MIN) p3 = INT_MAX;
//            if (std::min(p3, std::min(p1, p2)) != actual[i] - 1) {
//                std::cerr << "Incorrect distance: D: " << actual[i] << ", ";
//                std::cerr << "Xs: " << x1 << " " << x2 << " " << x3 << ",";
//                std::cerr << "XsSum: " << (x1 + x2 + x3) << "\n";
//                exit(666);
//            }
//        }
//    }
}


void withTime(std::string name, int repeat, graph& gr, const std::function<std::vector<int>(graph&)>& block,
              const std::vector<int>& expected,
              bool check_correctness = false) {
    std::cout << "Block: " << name << '\n';

    std::vector<long long> times(repeat);

    std::vector<int> res(repeat);

    for (int i = 0; i < repeat; ++i) {
        chrono_time::steady_clock::time_point start, end;
        start = chrono_time::steady_clock::now();
        res = block(gr);
        end = chrono_time::steady_clock::now();

        times[i] = chrono_time::duration_cast<chrono_time::milliseconds>(end - start)
                .count();

        if (check_correctness) {
            verify(expected, res, gr);
        }
    }

    long long total = 0;
    for (auto& time: times) {
        total += time;
    }
    std::cout << "\tAvg: " << ((double) total / times.size()) << '\n';

}

int main() {
    int times = 5;

    std::list<std::tuple<std::string, bool, std::vector<int> (*)(graph&, int)>> algos;
    algos.emplace_back("sequintal", false, sequintal::find_distances);
    algos.emplace_back("parallel", true, [](graph& graph, int start) {
        return parallel::find_distances(graph, start, false);
    });
    algos.emplace_back("parallel with parallel scan", true, [](graph& graph, int start) {
        return parallel::find_distances(graph, start, true);
    });

    omp_set_num_threads(omp_get_thread_limit());

    graph graph(500);
    std::vector<int> expected = sequintal::find_distances(graph, 0);
//    std::vector<int> expected;

    for (auto& p: algos) {
        std::string name = std::get<0>(p);
        bool check_correctness = std::get<1>(p);
        auto bfs = std::get<2>(p);
        std::vector<int> result;
        withTime(name,
                 times,
                 graph,
                 [&bfs](class graph& graph) { return bfs(graph, 0); },
                 expected,
                 check_correctness
        );
    }
    return 0;
}
