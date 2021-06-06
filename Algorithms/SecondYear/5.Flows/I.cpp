#include <algorithm>
#include <iostream>

using namespace std;
class Player {
  long long rock, paper, scissor;

public:
  long long minimizeWin(Player other) const {
    return max(0LL, max(other.paper - paper - scissor,
                        max(other.rock - rock - paper,
                            other.scissor - scissor - rock)));
  }
  Player(long long rock, long long scissor, long long paper) {
    this->rock = rock;
    this->paper = paper;
    this->scissor = scissor;
  }

};

int main() {
  long long a, b, c;
  cin >> a >> b >> c;
  Player rostislav(a, b, c);
  cin >> a >> b >> c;
  Player miroslav(a, b, c);
  cout << miroslav.minimizeWin(rostislav);
}
