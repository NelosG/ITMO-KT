#include "commons.h"


namespace sequintal {
    std::vector<int> scan(const std::vector<int>& array) {
        std::vector<int> result(array.size() + 1, 0);
        int i;

        result[1] = array[0];

        for (i = 1; i < array.size(); i++) {
            result[i + 1] = result[i] + array[i];
        }
        return result;
    }

    std::vector<int> filter(const std::vector<int>& array, const std::function<bool(int)>& pred) {
        std::vector<int> result;
        for (int i: array) {
            if (pred(i))
                result.push_back(i);
        }

        return result;
    }
}