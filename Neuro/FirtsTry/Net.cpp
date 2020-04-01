#include "Net.h"
#include <stdio.h>
Net::Net(int size) {
	this->size = size;
	neurons = (Neuron*)malloc(size * sizeof(Neuron));
	for (int i = 0; i < size; i++) {
		neurons[i].value = 1;
		neurons[i].wight = 1;
	}
	printf(")");
}
void Net::study(std::vector<std::vector<int>>arr) {
	for (int k = 0; k < 100; k++) {
		for (int i = 0; i < arr.size(); i++) {
			input(arr[i]);
			calc();
			double temp = cl();
			if (arr[i][arr[i].size() - 1] != porog(temp)) {
				for (int index = 0; index < answers.size(); index++) {
					neurons[index].wight += (arr[i][arr[i].size() - 1] - answers[index]) * n;
				}
			}
			printf("%f FF\n", temp);
		}
	}
}

int Net::ask(std::vector<int> arr) {
	input(arr);
	calc();
	return porog(cl());
}

double Net::cl() {
	double temp = 0;
	for (int i = 0; i < size; i++) {
		temp += answers[i];
	}
	printf("\n%f temp/size\n\n", neurons[0].activation(temp / size));
	return neurons[0].activation(temp / size);
}

int Net::porog(double x) {
	return x > 0.5 ? 1 : 0;
}
void Net::calc() {
	answers.clear();
	for (int i = 0; i < size; i++) {
		answers.push_back(neurons[i].calc());
		printf("%f answer%i\n", answers[i], i);
	}
	printf("--------\n");

}
void Net::input(std::vector<int>arr) {
	for (int i = 0; i < arr.size() - 1; i++) {
		neurons[i].value = arr[i];
	}
}
