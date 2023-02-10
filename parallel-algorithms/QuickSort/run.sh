#!/bin/bash
bash build.sh
cd build || exit
export OMP_THREAD_LIMIT=4
./quick_sort
