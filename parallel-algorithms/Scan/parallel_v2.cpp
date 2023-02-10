#include "scan.h"
#include <vector>
#include <omp.h>
#include <cmath>
#include <iostream>


namespace parallel_v2 {
    template<typename T>
    void omp_scan(int n, const T* in, T* out);

    void scan(const std::vector<long long>& array, std::vector<long long>& result) {
        omp_scan(array.size(), array.data(), result.data());
    }


    template<typename T>
    void omp_scan(int n, const T* in, T* out) {
        int i, chunk;
        const int num_threads = omp_get_max_threads();          // To get number of threads in machine

        long long last_value_chunk_array[num_threads + 1];
        /* Parallel region begins */
        #pragma omp parallel shared(in, out, chunk, num_threads) private(i)
        {
            chunk = ceil(n / (double) num_threads);                           // Defining chunk values
            const int idthread = omp_get_thread_num();              // Get thread ID
            #pragma omp single
            {
                last_value_chunk_array[0] = 0;
            }

            long long operation = 0;
            /* For region begins */
            #pragma omp for schedule(static, chunk) nowait
            for (i = 0; i < n; i++) {
                if ((i % chunk) == 0) {
                    operation = in[i];                      // Breaking at every chunk
                    out[i] = in[i];
                } else {
                    out[i] = out[i - 1] + in[i];           // Performing the required operation
                    operation = operation + in[i];
                }
            }
            /* For region ends */
            last_value_chunk_array[idthread +
                                   1] = operation;         // Assigning sums of all chunks in last_chunk_value array

            #pragma omp barrier                                     // Syncing all the threads

            long long balance = last_value_chunk_array[1];                // Initialising with index 1 value as for thread 0, result has already been calculated

            for (i = 2; i < (idthread + 1); i++)
                balance = balance + last_value_chunk_array[i];    // Creating balance for every thread

            #pragma omp for schedule(static, chunk)                 // To calculate the sum of all chunks
            for (i = 0; i < n; i++) {
                if (idthread != 0) {
                    out[i] = out[i] + balance;                   // For thread IDs other than 0
                }
            }
        }
        /* Parallel region ends */
    } // omp_scan
}
