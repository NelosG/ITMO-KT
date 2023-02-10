cmake_minimum_required(VERSION 3.16)
project(quick_sort CXX)

set(CMAKE_CXX_STANDARD 17)

add_executable(quick_sort
        main.cpp
        quick_sort.h
        sequintal.cpp
        standard.cpp
        parallel.cpp
        parallel_v2.cpp
        )

find_package(OpenMP)
if (OpenMP_CXX_FOUND)
    target_link_libraries(quick_sort PUBLIC OpenMP::OpenMP_CXX)
else ()
    message(OPEN_MP not found)
endif ()
