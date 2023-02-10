#include "quick_sort.h"
#include <algorithm>
#include <ctime>
#include <random>

namespace parallel_v2 {

    void qsort_tasks(std::vector<int>& array, int low, int high, std::mt19937& gen);

    void qsort(std::vector<int>& array) {
        std::mt19937 gen(clock());
        qsort_tasks(array, 0, (int) array.size() - 1, gen);
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

    void qsort(std::vector<int>& array, int low, int high, std::mt19937& gen) {
        if (low < high) {
            int q = partition_r(array, low, high, gen);

            if (high - low < BLOCK) {
                qsort(array, low, q - 1, gen);
                qsort(array, q + 1, high, gen);
            } else {
#pragma omp task default(none) shared(array, gen, q, low)
                qsort(array, low, q - 1, gen);
#pragma omp task default(none) shared(array, gen, q, high)
                qsort(array, q + 1, high, gen);
            }
        }
    }

    void qsort_tasks(std::vector<int>& array, int low, int high, std::mt19937& gen) {
#pragma omp parallel default(none) shared(array, low, high, gen)
        {
#pragma omp single
            qsort(array, low, high, gen);
#pragma omp taskwait
        }
    }
}
