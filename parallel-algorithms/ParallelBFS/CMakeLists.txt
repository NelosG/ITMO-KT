cmake_minimum_required(VERSION 3.23)
project(ParallelBFS)

set(CMAKE_CXX_STANDARD 20)

add_executable(parallel-bfs
        main.cpp
        bfs.h
        bfs-sequintal.cpp
        bfs-parallel.cpp
        common/commons.h
        common/parallel.cpp
        common/sequintal.cpp
        graph/graph.h)

find_package(OpenMP)

if (OpenMP_CXX_FOUND)
    target_link_libraries(parallel-bfs PUBLIC OpenMP::OpenMP_CXX)
else ()
    message(OPEN_MP not found)
endif ()
