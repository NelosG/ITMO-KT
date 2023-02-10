#include "scan.h"
#include <vector>
#include <omp.h>


namespace parallel {
    void first(int l, int r, Tree *node, const std::vector<long long> &array) {
        int m = (l + r) / 2;

        if (r - l == 1) {
            node->value = array[l];
        } else if (r - l < BLOCK) {
            node->l = new Tree();
            node->r = new Tree();
            first(l, m, node->l, array);
            first(m, r, node->r, array);
            node->value = node->l->value + node->r->value;
        } else {
#pragma omp parallel sections shared(array, node) num_threads(2)
            {
#pragma omp section
                {
                    node->l = new Tree();
                    first(l, m, node->l, array);
                }
#pragma omp section
                {
                    node->r = new Tree();
                    first(m, r, node->r, array);
                }
            }
            node->value = node->l->value + node->r->value;
        }
    }

    void
    second(int l, int r, long long prefix, Tree *node, const std::vector<long long> &array, std::vector<long long> &result) {
        int m = (l + r) / 2;

        if (r - l == 1) {
            result[l] = prefix + array[l];
        } else if (r - l < BLOCK) {
            second(l, m, prefix, node->l, array, result);
            second(m, r, prefix + node->l->value, node->r, array, result);
            delete node->l;
            delete node->r;
        } else {
#pragma omp parallel sections shared(array, node) num_threads(2)
            {
#pragma omp section
                {
                    second(l, m, prefix, node->l, array, result);
                }
#pragma omp section
                {
                    second(m, r, prefix + node->l->value, node->r, array, result);
                }
            }
            delete node->l;
            delete node->r;
        }
    }
    void scan(const std::vector<long long>& array, std::vector<long long>& result) {
        auto node = new Tree;
        first(0, (int)array.size(), node, array);
        second(0, (int)array.size(), 0, node, array, result);
        delete node;
    }
}
