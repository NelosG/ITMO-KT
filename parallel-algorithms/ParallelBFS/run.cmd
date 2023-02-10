build.cmd | more
cd build
set OMP_THREAD_LIMIT=4
parallel-bfs.exe
Pause
