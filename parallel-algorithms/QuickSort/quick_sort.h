#pragma once

#include <vector>

#define BLOCK 1'000

namespace standard {
    void sort(std::vector<int> &array);
    void qsort(std::vector<int> &array);
}

namespace sequintal {
    void qsort(std::vector<int>& array);
}

namespace parallel {
    void qsort(std::vector<int>& array);
}

namespace parallel_v2 {
    void qsort(std::vector<int>& array);
}
