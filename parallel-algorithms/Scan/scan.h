#ifndef SCAN_SCAN_H
#define SCAN_SCAN_H
#include <vector>

#define BLOCK 10'000

class Tree {
public:
    long long value = 0;
    Tree* l = nullptr;
    Tree* r = nullptr;
};

namespace sequintal {
    void scan(const std::vector<long long>& array, std::vector<long long>& result);
}

namespace sequintal_v2 {
    void scan(const std::vector<long long>& array, std::vector<long long>& result);
}

namespace parallel {
    void scan(const std::vector<long long>& array, std::vector<long long>& result);
}

namespace parallel_v2 {
    void scan(const std::vector<long long>& array, std::vector<long long>& result);
}


#endif //SCAN_SCAN_H
