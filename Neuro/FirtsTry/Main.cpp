#include <iostream>
#include "Net.h"
#include <vector>
int main() {
	std::vector<std::vector<int>> arr;
	std::vector<int> temp;
	temp.push_back(1);
	temp.push_back(0);
	temp.push_back(1);
	arr.push_back(temp);
	temp.clear();
	temp.push_back(1);
	temp.push_back(1);
	temp.push_back(1);
	arr.push_back(temp);
	temp.clear(); 
	temp.push_back(0);
	temp.push_back(0);
	temp.push_back(0);
	arr.push_back(temp);
	temp.clear();
	temp.push_back(0);
	temp.push_back(1);
	temp.push_back(0);
	arr.push_back(temp);
	temp.clear();
	Net net(2);
	net.study(arr);
	temp.push_back(1);
	temp.push_back(0);
	temp.push_back(0);

	printf("\n++++++++++++++= %i\n\n", temp.size());
	int t = net.ask(temp);
	std::cout << t;
	return 0;
}