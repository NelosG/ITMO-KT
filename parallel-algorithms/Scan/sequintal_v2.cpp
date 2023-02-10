#include "scan.h"
#include <vector>
#include <cmath>
#include <iostream>


namespace sequintal_v2 {
    void scan(const std::vector<long long>& array, std::vector<long long>& result) {
        int i;

        long long operation = array[0];
        result[0] = array[0];

        /* For region begins */
        for (i = 1; i < array.size(); i++) {
            result[i] = result[i - 1] + array[i];           // Performing the required operation
            operation = operation + array[i];
        }
    }

}
