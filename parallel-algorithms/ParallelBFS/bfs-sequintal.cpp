#include "bfs.h"

#include <list>
#include <queue>

namespace sequintal {
    std::vector<int> find_distances(graph& graph, int start){
        std::queue<int> queue;
        std::vector<int> dist(graph.size, 0);
        std::vector<bool> visited(graph.size, false);

        queue.push(start);
        visited[start] = true;
        dist[start] = 0;

        while (!queue.empty()) {
            int v = queue.front();
            queue.pop();

            for (auto to : graph.neighbour_nodes(v)) {
                if (!visited[to]) {
                    visited[to] = true;
                    queue.push(to);
                    dist[to] = dist[v] + 1;
                }
            }
        }
        return dist;
    }

}