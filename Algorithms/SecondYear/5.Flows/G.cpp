#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;
using Long = long long;

class Edge {
  Long capacity;

public:
  Long currentCapacity() { return capacity - flow; }
  Long johnsonCost(vector<Long> &potentials) const {
    return weight + potentials[from] - potentials[to];
  }
  Edge(int from, int to, Long capacity, Long weight, int reverseIndex) {
    this->from = from;
    this->to = to;
    this->capacity = capacity;
    this->weight = weight;
    this->reverseIndex = reverseIndex;
  }
  int from;
  int to;
  int reverseIndex;
  Long flow = 0;
  Long weight;
};

class Network {
  Long INF = INT64_MAX;
  int n, start, finish;
  vector<vector<Edge>> edges;
  vector<Long> potentials;

public:
  explicit Network(int n) {
    this->n = n;
    this->start = 0;
    this->finish = n - 1;
    potentials.assign(n, 0);
    for (int i = 0; i < n; ++i)
      edges.emplace_back(vector<Edge>());
  }

  void addOrientedEdge(int from, int to, Long capacity, Long weight) {
    int reverse_from_index = edges[to].size();
    int reverse_to_index = edges[from].size();
    edges[from].emplace_back(from, to, capacity, weight, reverse_from_index);
    edges[to].emplace_back(to, from, 0, -weight, reverse_to_index);
  }

  Long findMaxFlowMinCost() {
    vector<bool> reached(n, false);
    vector<Edge *> path(n, nullptr);
    Long result = 0;

    while (true) {
      vector<Long> distance = dijkstra(path, reached);
      if (!reached[finish])
        break;
      for (int i = 0; i < n; ++i) {
        potentials[i] = reached[i] ? potentials[i] + distance[i] : 0;
      }

      Long flow = INF;
      for (int v = finish; v != start; v = path[v]->from)
        flow = min(flow, path[v]->currentCapacity());

      for (int v = finish; v != start; v = path[v]->from) {
        Edge* e = path[v];
        Edge* backwardEdge = &edges[e->to][e->reverseIndex];
        e->flow += flow;
        backwardEdge->flow -= flow;
        result += flow * e->weight;
      }
    }
    return result;
  }

  vector<Long> dijkstra(vector<Edge *> &path, vector<bool> &reached) {
    vector<Long> distance(n, INF);
    reached.assign(reached.size(), false);
    path.assign(path.size(), nullptr);

    distance[start] = 0;

    while (true) {
      int nearestVertex = -1;

      vector<pair<Long, int>> temp;

      for (int i = 0; i < n; ++i) {
        if (!reached[i] && distance[i] < INF) {
          temp.emplace_back(distance[i], i);
        }
      }
      if(!temp.empty()) {
        sort(temp.begin(), temp.end());
        nearestVertex = temp.begin()->second;
      }

      if (nearestVertex == -1)
        break;
      reached[nearestVertex] = true;

      for(auto & i : edges[nearestVertex])
        if (i.currentCapacity() > 0 && !reached[i.to])
          if (distance[i.to] > distance[i.from] + i.johnsonCost(potentials)) {
            distance[i.to] = distance[i.from] + i.johnsonCost(potentials);
            path[i.to] = &i;
          }
    }
    return distance;
  }
};

int main() {
  int n, m;
  cin >> n >> m;
  Network network(n);
  for (int i = 0, a, b, c, d; i < m; ++i) {
    cin >> a >> b >> c >> d;
    network.addOrientedEdge(a - 1, b - 1, c, d);
  }
  cout << network.findMaxFlowMinCost();
}
