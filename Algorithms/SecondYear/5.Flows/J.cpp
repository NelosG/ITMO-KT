#pragma intrinsic(_disable, _outp, fabs, strcmp, _enable, _outpw, labs, strcpy, _inp, _rotl, memcmp, strlen, _inpw, _rotr, memcpy, _lrotl,_strset, memset, _lrotr, abs, strcat)
#pragma optimize("gty", on)
#pragma inline_recursion(on)

#include <algorithm>
#include <iostream>
#include <queue>
#include <vector>

using namespace std;
using Long = long long;

class Edge {
  Long capacity;

public:
  inline Long currentCapacity()  { return capacity - flow; }
  inline Long johnsonCost(vector<Long> &potentials) const {
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
public:
  Long INF = INT64_MAX;

  int n, start, finish;
  vector<vector<Edge>> edges;
  vector<Long> potentials;

  inline Long findMaxFlowMinCost() {
    vector<Edge *> path(n, nullptr);
    Long result = 0;

    while (true) {
      vector<Long> distance = quickDijkstra(path);
      if (distance[finish] == INF)
        break;
      for (int i = 0; i < n; ++i) {
        potentials[i] = distance[i] != INF ? potentials[i] + distance[i] : 0;
      }

      Long flow = INF;
      for (int v = finish; v != start; v = path[v]->from)
        flow = min(flow, path[v]->currentCapacity());

      for (int v = finish; v != start; v = path[v]->from) {
        Edge *e = path[v];
        Edge *backwardEdge = &edges[e->to][e->reverseIndex];
        e->flow += flow;
        backwardEdge->flow -= flow;
        result += flow * e->weight;
      }
    }
    return result;
  }

inline void addOrientedEdge(int from, int to, Long capacity, Long weight) {
    int reverse_from_index = edges[to].size();
    int reverse_to_index = edges[from].size();
    edges[from].push_back(Edge(from, to, capacity, weight, reverse_from_index));
    edges[to].push_back(Edge(to, from, 0, -weight, reverse_to_index));
  }
  explicit Network(int n) {
    this->n = n;
    this->start = 0;
    this->finish = n - 1;
    potentials.assign(n, 0);
    for (int i = 0; i < n; ++i)
      edges.emplace_back(vector<Edge>());

  }
  inline vector<Long> quickDijkstra(vector<Edge *> &path)  {
    vector<Long> distance(n, INF);
    path.assign(path.size(), nullptr);

    distance[start] = 0;
    priority_queue<pair<Long, int>, vector<pair<Long, int>>, greater<>> q;
    for (Edge &e : edges[start])
      if (e.currentCapacity() > 0) {
        distance[e.to] = e.johnsonCost(potentials);
        path[e.to] = &e;
      }

    for (int v = 0; v < n; ++v)
      if (v != start)
        q.push({distance[v], v});

    while (!q.empty()) {
      pair<Long, int> way = q.top();
      q.pop();
      Long curMinPath = way.first;
      int v = way.second;

      if (distance[v] < curMinPath || distance[v] == INF)
        continue;


      for(auto & i : edges[v]) {
        if (i.currentCapacity() > 0)
          if (distance[i.to] > distance[i.from] + i.johnsonCost(potentials)) {
            distance[i.to] = distance[i.from] + i.johnsonCost(potentials);
            path[i.to] = &i;
            q.push({distance[i.from] + i.johnsonCost(potentials), i.to});
          }
      }
    }
    return distance;
  }
};

int main() {
  ios_base::sync_with_stdio(false);
  cin.tie(nullptr);
  cout.tie(nullptr);

  int n, m;
  cin >> n >> m;
  Network network(2 * n + 2);

  for (int i = 1; i <= n; ++i) {
    Long weight;
    cin >> weight;
    network.addOrientedEdge(network.start, i, 1, 0);
    network.addOrientedEdge(i, n + i, 1, weight);
    network.addOrientedEdge(i + n, i, n, 0);
    network.addOrientedEdge(i + n, network.finish, 1, 0);
  }
  for (int i = 0; i < m; ++i) {
    int to, from;
    Long weight;
    cin >> from >> to >> weight;
    network.addOrientedEdge(from, n + to, m, weight);
  }


  cout << network.findMaxFlowMinCost();
}
