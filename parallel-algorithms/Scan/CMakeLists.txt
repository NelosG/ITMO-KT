cmake_minimum_required(VERSION 3.23)
project(Scan)

set(CMAKE_CXX_STANDARD 17)

add_executable(Scan
               main.cpp
               scan.h
               parallel.cpp
               sequintal.cpp parallel_v2.cpp sequintal_v2.cpp)

find_package(OpenMP)
if(OpenMP_CXX_FOUND)
    target_link_libraries(Scan PUBLIC OpenMP::OpenMP_CXX)
else()
    message(OPEN_MP not found)
endif()
