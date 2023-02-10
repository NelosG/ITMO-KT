//
// Created by glebp on 23.12.2022.
//

#ifndef PARALLELBFS_BFS_H
#define PARALLELBFS_BFS_H

#include "graph/graph.h"

namespace sequintal {
    std::vector<int> find_distances(graph& graph, int start);
}

namespace parallel {
    std::vector<int>  find_distances(graph& graph, int start, bool is_parallel);
}

#endif //PARALLELBFS_BFS_H
