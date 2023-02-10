#include <algorithm>
#include "quick_sort.h"


void standard::sort(std::vector<int>& array) {
    std::sort(array.begin(), array.end());
}

void standard::qsort(std::vector<int>& array) {
    std::qsort(
            array.data(),
            array.size(),
            sizeof(int),
            [](const void* x, const void* y) {
                const int arg1 = *static_cast<const int*>(x);
                const int arg2 = *static_cast<const int*>(y);
                return arg1 < arg2 ? -1 : (arg1 > arg2 ? 1 : 0);
            });
}
