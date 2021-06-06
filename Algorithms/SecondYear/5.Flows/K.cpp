#pragma intrinsic(_disable, _outp, fabs, strcmp, _enable, _outpw, labs, strcpy, _inp, _rotl, memcmp, strlen, _inpw, _rotr, memcpy, _lrotl,_strset, memset, _lrotr, abs, strcat)
#pragma optimize("gty", on)
#pragma inline_recursion(on)
#include <algorithm>
#include <iomanip>
#include <iostream>
#include <set>
#include <vector>
#include <sstream>

using namespace std;

using Long = long long;
const Long INF = INT64_MAX;

struct Edge {
  Long c{}, from{}, to{}, num{}, revNum{}, f{}, cost{}, index{};
  Edge(const Edge &other) = default;

  Edge(Long c, Long f, Long to, Long num, Long cost, Long revNum, Long from,
       Long index)
      : c(c), f(f), to(to), num(num), cost(cost), revNum(revNum), from(from),
        index(index) {}
};

inline pair<pair<Long, bool>, vector<Long>>
dijkstra(vector<vector<Long>> &g, Long s, Long &t, vector<Edge> &edgesList) {
  set<pair<Long, Long>> q;
  vector<Long> d(g.size(), INF);
  vector<pair<Long, Long>> parent(g.size());

  d[s] = 0;
  parent[s] = {-1, -1};
  q.insert({s, 0});

  while (!q.empty()) {
    Long v = (*q.begin()).second;
    q.erase(q.begin());

    for (Long it : g[v]) {
      Edge e = edgesList.at(it);
      if (d.at(v) + e.cost < d.at(e.to) && e.f < e.c) {
        q.erase({d[e.to], e.to});
        d.at(e.to) = d.at(v) + e.cost;
        q.insert({d.at(e.to), e.to});
        parent.at(e.to) = {v, it};
      }
    }
  }
  pair<pair<Long, bool>, vector<Long>> p;

  bool &hasPath = p.first.second;
  hasPath = (d.at(t) != INF);
  if (!hasPath) {
    return {{0, hasPath}, vector<Long>()};
  }

  vector<Long> &path = p.second;
  pair<Long, Long> v = parent[t];
  while (v.first != -1) {
    path.push_back(v.second);
    v = parent.at(v.first);
  }
  reverse(path.begin(), path.end());
  Long &minCapacity = p.first.first;
  minCapacity = INF;

  for (Long it : path) {
    minCapacity = min(minCapacity, edgesList[it].c - edgesList[it].f);
  }
  return p;
}

inline bool minCostFlow(vector<vector<Long>> &g, vector<Edge> &edgesList, Long s,
                 Long t, Long k) {
  int cnt = 0;
  while (true) {
    pair<pair<Long, bool>, vector<Long>> res = dijkstra(g, s, t, edgesList);
    Long &minCapacity = res.first.first;
    bool &hasPath = res.first.second;
    if (!hasPath) {
      return cnt == k;
    }

    if (cnt >= k) {
      return true;
    }

    vector<Long> &edgesInPath = res.second;

    for (Long &eInd : edgesInPath) {
      edgesList.at(eInd).f += minCapacity;
      edgesList.at(edgesList.at(eInd).revNum).f -= minCapacity;
    }
    cnt++;
  }
}

bool pathFound = false;
inline void findPath(Long v, Long t, vector<vector<Long>> &g, vector<Edge> &edgesList,
              vector<Long> &path) {
  if (v == t) {
    pathFound = true;
    return;
  }

  for (Long i = 0; i < g[v].size(); i++) {
    Edge &e = edgesList[g[v][i]];
    if (!pathFound && e.f == 1) {
      edgesList[g[v][i]].f = 0;
      path.push_back(e.index);
      findPath(e.to, t, g, edgesList, path);
    }
  }
}

int main() {
  ios_base::sync_with_stdio(false);
  cin.tie(nullptr);
  cout.tie(nullptr);

  Long n, m, k;
  cin >> n >> m >> k;

  vector<vector<Long>> g(n);
  vector<Edge> edgesList;

  for (Long i = 0, u, v, cost; i < m; i++) {
    cin >> u >> v >> cost;
    --u, --v;

    edgesList.emplace_back(1, 0, v, edgesList.size(), cost,
                           edgesList.size() + 1, u, i);
    g.at(u).push_back(edgesList.back().num);
    edgesList.emplace_back(0, 0, u, edgesList.size(), -cost,
                           edgesList.back().num, v, i);
    g.at(v).push_back(edgesList.back().num);
    edgesList.emplace_back(1, 0, u, edgesList.size(), cost,
                           edgesList.size() + 1, v, i);
    g.at(edgesList.back().from).push_back(edgesList.back().num);
    edgesList.emplace_back(0, 0, v, edgesList.size(), -cost,
                           edgesList.back().num, edgesList.back().to, i);
    g.at(edgesList.back().from).push_back(edgesList.back().num);
  }

  if (!minCostFlow(g, edgesList, 0, n - 1, k)) {
    cout << -1;
    return 0;
  }

  stringstream ss;
  Long ans = 0;
  for (Long i = 0; i < edgesList.size(); i += 2) {
    ans += (edgesList.at(i).f * edgesList.at(i).cost);
  }
  ss << fixed << setprecision(5);
  ss << (double)ans / (double)k << endl;

  for (Long i = 0; i < k; i++) {
    pathFound = false;
    vector<Long> path;
    findPath(0, n - 1, g, edgesList, path);

    ss << path.size() << " ";
    for (Long &it : path) {
      ss << (it + 1) << " ";
    }
    ss << '\n';
  }
  cout << ss.str();
}