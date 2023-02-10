#pragma clang diagnostic push

#include "quick_sort.h"
#include <algorithm>
#include <ctime>
#include <random>

namespace parallel {
    void quick_sort(std::vector<int>& array, int low, int high, std::mt19937& gen);

    void qsort(std::vector<int>& array) {
        std::mt19937 gen(clock());
        quick_sort(array, 0, (int) array.size() - 1, gen);
    }

    int partition(std::vector<int>& array, int low, int high) {
        int pivot = array[high];

        int i = (low - 1);
        for (int j = low; j <= high - 1; j++) {
            if (array[j] <= pivot) {
                i++;
                std::swap(array[i], array[j]);
            }
        }
        std::swap(array[i + 1], array[high]);
        return (i + 1);
    }

    int partition_r(std::vector<int>& array, int low, int high, std::mt19937& gen) {
        int mod = (high - low);
        int random = low + ((int) gen() % mod + mod) % mod;
        std::swap(array[random], array[high]);

        return partition(array, low, high);
    }

    void quick_sort(std::vector<int>& array, int low, int high, std::mt19937& gen) {
        if (low < high) {
            int pi = partition_r(array, low, high, gen);

            if (high - low < BLOCK) {
                quick_sort(array, low, pi - 1, gen);
                quick_sort(array, pi + 1, high, gen);
            } else {

#pragma omp parallel sections default(none) shared(array, gen, pi, low, high) num_threads(2)
                {
#pragma omp section
                    quick_sort(array, low, pi - 1, gen);
#pragma omp section
                    quick_sort(array, pi + 1, high, gen);
                }
            }
        }
    }
}

#pragma clang diagnostic pop
