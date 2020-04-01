#pragma once
#include "Neuron.h"
#include <vector>

class Net {
public:
	Net(int size);
	Neuron* neurons;
	std::vector<double> answers;
	int size;
	double n = 0.1;
	void study(std::vector<std::vector<int>> arr);
	int ask(std::vector<int> arr);
	double cl();
	int porog(double x);
	void calc();
	void input(std::vector<int> arr);
};