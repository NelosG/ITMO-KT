#include<cuda_runtime.h>
#include<iostream>
#include<ctime>
#include<thread>

__global__ void kernel() {
	long long temp = 1000000000000;

	while (temp != 0) {
		temp--;
	}
}

void one() {
	long long temp = 1000000000000;

	while (temp != 1) {
		temp--;
	}
}

int main(void) {
	cudaError_t cudaStatus = cudaSetDevice(0);
	if (cudaStatus != cudaSuccess) {
		fprintf(stderr, "Cuda failed!");
		return 1;
	}
	long start = clock();
	for (long i = 0; i < 1200000; i++) {
		kernel <<<1, 896 >>> ();
	}
	long end = clock();
	long temp = end - start;
	std::cout << temp << "   ms  896 Threads on GPU\n";
	start = clock();
	for (int i = 0; i < 8960; i++) {
		std::thread thr1(one);
		std::thread thr2(one);
		std::thread thr3(one);
		std::thread thr4(one);
		std::thread thr5(one);
		std::thread thr6(one);
		std::thread thr7(one);
		std::thread thr8(one);
		std::thread thr9(one);
		std::thread thr11(one);
		std::thread thr12(one);
		thr1.join();
		thr2.join();
		thr3.join();
		thr4.join();
		thr5.join();
		thr6.join();
		thr7.join();
		thr8.join();
		thr9.join();
		thr11.join();
		thr12.join();
	}

	end = clock();
	long temp1 = (end - start) * 100000;
	std::cout << temp1 << "    ms  12 Threads by CPU\n\n";

	std::cout << temp1 / temp << "  Times faster\n\n\n\n";
	std::cout << "Task: Do  " << 1000000000000 << "  subtractions  " << (1200000 * 896) << "  times\n\n\n\n\n";
	std::cin >> temp; // Что бы консоль не закрывалась если вы запускаете .exe
}
