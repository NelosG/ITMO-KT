
#ifndef PARALLELBFS_COMMONS_H
#define PARALLELBFS_COMMONS_H

#include <vector>
#include <functional>


namespace sequintal {
    std::vector<int> scan(const std::vector<int>& array);

    std::vector<int> filter(const std::vector<int>& array, const std::function<bool (int)>& predicate);
}

namespace parallel {
#define THRESHOLD 5'000

    std::vector<int> scan(const std::vector<int>& array);

    std::vector<int> filter(const std::vector<int>& array, const std::function<bool (int)>& predicate);
}


#endif //PARALLELBFS_COMMONS_H
