#include "bfs.h"
#include "common/commons.h"
#include <atomic>
#include <algorithm>
#include <functional>
#include <climits>
#include <iostream>


namespace parallel {
    std::vector<int> find_distances(graph& graph, int start, bool is_parallel) {
        std::function<std::vector<int>(const std::vector<int>&)> scan;
        std::function<std::vector<int>(const std::vector<int>&, const std::function<bool(int)>&)> filter;

        filter = sequintal::filter;
        if (is_parallel) {
            scan = parallel::scan;
        } else {
            scan = sequintal::scan;
        }

        std::vector<int> dist(graph.size, INT_MAX);
        std::vector<std::atomic_bool> visited(graph.size);
        std::vector<int> front;
        front.push_back(start);

        visited[start] = true;
        dist[start] = 0;


        int cur_dist = 0;
        while (!front.empty()) {
            cur_dist++;
            std::vector<int> deg(front.size());

            #pragma omp parallel for default(none) shared(front, deg, graph)
            for (int i = 0; i < deg.size(); ++i) {
                deg[i] = graph.neighbour_nodes_count(front[i]);
            }

            deg = scan(deg);
            int size = deg[deg.size() - 1];

            if (size < 0) {
                for (auto d: deg) {
                    std::cout << d << "\n";
                }
            }
            std::vector<int> next_front(size, INT_MAX);

            #pragma omp parallel for default(none) shared(front, graph, visited, cur_dist, dist, next_front, deg)
            for (int i = 0; i < front.size(); ++i) {
                auto nextNodes = graph.neighbour_nodes(front[i]);
                for (int j = 0; j < nextNodes.size(); ++j) {
                    auto to = nextNodes[j];
                    auto expected = false;
                    if (visited[to].compare_exchange_strong(expected, true)) {
                        dist[to] = cur_dist;
                        next_front[deg[i] + j] = to;
                    }
                }
            }
            front.resize(next_front.size());
            auto it = std::copy_if(next_front.begin(), next_front.end(), front.begin(),
                                   [](int i) { return i < INT_MAX; });
            front.resize(std::distance(front.begin(), it));
//            front = filter(next_front, [](int i) { return i < INT_MAX; });
        }
        return dist;
    }

}