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
./quick_sort
```

or run ```run.sh```

В случае если не установить ```OMP_THREAD_LIMIT=4``` версия "parallel" может работать на большем кол-ве потоков

С версией "palallel_v2" все ок, лучше проверять ее.

# Что сделано

Параллелим рекурсивные вызовы если больше определенного размера(BLOCK)

# Результаты запуска (Среднее на 5 разных массивах)

## Софт

- Windows 11
- MingW-w64 9.0

## Процессор

- Intel i7-9750H, 6 реальных/12 виртуальных ядер

## Однопоточные

- Average std::sort **8252ms**
- Average std::qsort **14675ms**
- Average sequintal **10066ms**
- Average parallel **10156ms**
- Average parallel_v2 **10063ms**

## На 10^8 в 4 потоках

- Average parallel **5562ms**
- Average parallel_v2 **3558ms**

# Результаты запуска на другом процессоре:

## Процессор

- Intel i9-12900H, 14 реальных/20 виртуальных ядер

## Однопоточные

- Average std::sort **6603ms**
- Average std::qsort **11969ms**
- Average sequintal **7992ms**
- Average parallel **7976ms**
- Average parallel_v2 **7967ms**

## 4 потока

- Average parallel **4584ms**
- Average parallel_v2 **3116ms**

# Доп. запуски на 1-м процессоре(i7)

## 2 потока

- Average parallel **7431ms**
- Average parallel_v2 **5264ms**

## 6 потоков

- Average parallel **4844ms**
- Average parallel_v2 **2896ms**

## 8 потоков

- Average parallel **4282ms**
- Average parallel_v2 **2550ms**

## 10 потоков

- Average parallel **3897ms**
- Average parallel_v2 **2418ms**

## 12 потоков

- Average parallel **3343ms**
- Average parallel_v2 **2219ms**

# Доп. запуски на 2-м процессоре(i9)

## 2 потока

- Average parallel **6624ms**
- Average parallel_v2 **4490ms**

## 8 потоков

- Average parallel **3473ms**
- Average parallel_v2 **2723ms**

## 16 потоков

- Average parallel **3897ms**
- Average parallel_v2 **2425ms**

## 20 потоков

- Average parallel **2977ms**
- Average parallel_v2 **2199ms**
