# Build

```
cmake -S . -B build/ -D CMAKE_BUILD_TYPE=Release
cmake --build build/
```

or run ```build.sh```

# Run

```
cd build
export OMP_THREAD_LIMIT=4
./parallel-bfs
```

or run ```run.sh```

# Results
## Sequintal
Avg: 22803.2
## Parallel
Avg: 8207.6
## Parallel with parallel scan
Avg: 8458.2