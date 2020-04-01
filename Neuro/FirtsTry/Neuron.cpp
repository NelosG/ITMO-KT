#include "neuron.h"
#include <cmath>
#include <stdio.h>
Neuron::Neuron(double value) {
	this->value = value;
}

double Neuron::calc() {
	double temp = activation((value * wight));
	printf("%f value, %f wight, %f temp\n",value, wight,  temp);
	return temp;
}

double Neuron::activation(double x)
{
	return 1 - pow(tanh(x),2);
}
