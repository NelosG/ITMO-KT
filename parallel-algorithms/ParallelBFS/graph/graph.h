//
// Created by glebp on 23.12.2022.
//

#ifndef PARALLELBFS_GRAPH_H
#define PARALLELBFS_GRAPH_H


#include <vector>

class graph {
public:
    graph(int n = 500) : n(n), size(n * n * n) {

//        for (int i = 0; i < size; ++i) {
//            int a, b, c;
//            use_mask(i, a, b, c);
//
//            int counter = 0;
//            if (validUIndex(a)) counter++;
//            if (validBIndex(a)) counter++;
//            if (validUIndex(b)) counter++;
//            if (validBIndex(b)) counter++;
//            if (validUIndex(c)) counter++;
//            if (validBIndex(c)) counter++;
//            nexts.push_back(counter);
//        }
    }


    std::vector<int> neighbour_nodes(int from) {
        int a, b, c;
        use_mask(from, a, b, c);
        std::vector<int> nodes;

        if (validUIndex(a)) nodes.push_back(toUId(a + 1, b, c));
        if (validBIndex(a)) nodes.push_back(toUId(a - 1, b, c));
        if (validUIndex(b)) nodes.push_back(toUId(a, b + 1, c));
        if (validBIndex(b)) nodes.push_back(toUId(a, b - 1, c));
        if (validUIndex(c)) nodes.push_back(toUId(a, b, c + 1));
        if (validBIndex(c)) nodes.push_back(toUId(a, b, c - 1));
        return nodes;
    }

        int neighbour_nodes_count(int from) {
        int a, b, c;
        use_mask(from, a, b, c);
        int nodes_size = 0;

        if (validUIndex(a)) ++nodes_size;
        if (validBIndex(a)) ++nodes_size;
        if (validUIndex(b)) ++nodes_size;
        if (validBIndex(b)) ++nodes_size;
        if (validUIndex(c)) ++nodes_size;
        if (validBIndex(c)) ++nodes_size;
        return nodes_size;
    }

    void use_mask(int v, int& a, int& b, int& c) const {
        a = v / (n * n);
        b = (v / n) % n;
        c = v % n;
    }


    bool validUIndex(int from) {
        return from < n - 1;
    }

    bool validBIndex(int from) {
        return from > 0;
    }

    int toUId(int a, int b, int c) {
        return a * n * n + b * n + c;
    }

    int size;

    int n ;
};


#endif //PARALLELBFS_GRAPH_H
