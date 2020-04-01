#pragma once
#include <vector>

class Neuron {
public:
	Neuron(double value);
	double wight;
	double value;
	double e = 2.71828182845;
	double activation(double x);
	double calc();
};