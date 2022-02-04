import seaborn as sns
from matplotlib import pyplot as plt
import numpy as np
from sklearn.cluster import KMeans
from sklearn.metrics.pairwise import euclidean_distances
from sklearn import metrics
from sklearn.metrics.cluster import rand_score

def print_info(X, y, X_reduced, clusters, str: str, to_Show: bool):
    kmeans = KMeans(n_clusters=clusters).fit(X_reduced)

    if to_Show:
        sns.scatterplot(x=X_reduced[:, 0],
                        y=X_reduced[:, 1],
                        hue=kmeans.labels_,
                        palette=sns.color_palette(n_colors=clusters))
        plt.show()
    print(f"---- {str} ----")

    y_pred = kmeans.predict(X_reduced)

    cohension = Cohension(X, y_pred)
    separation = Separation(X, y_pred)
    mean_distance_to_center = np.mean([euclidean_distances([kmeans.cluster_centers_[kmeans.labels_[idx]]],
                                                           [point]) for idx, point in enumerate(X_reduced)
                                       ])

    mean_center_distance = np.triu(euclidean_distances(kmeans.cluster_centers_), 1).mean()
    print(f"    Среднее Внутрикластерное растояние:  {mean_distance_to_center:}")
    print(f"    Среднее Межкластерное расстояние: {mean_center_distance}")
    print(f"    Внутрикластерное/Межкластерное: {mean_distance_to_center / mean_center_distance}\n")

    print("   ==Внешние меры==")
    print(f"    Rand_score: {rand_score(y, y_pred)}")
    # print(f"    Скорректированный индекс RAND: {metrics.adjusted_rand_score(y, kmeans.labels_)}")
    print(f"    F-Measure:   {F(X, y, y_pred)}")

    print("   ==Внутренние меры==")
    print(f"    Компактность кластеров:  {cohension:}")
    print(f"    Отделимость кластеров: {separation}")
    print(f"    Силуэт: {metrics.silhouette_score(X, y_pred, metric='euclidean')}")
    print(f"    Калински и Харабаз: {metrics.calinski_harabasz_score(X, y_pred)}")


def split_clasters(X, y_pred):
    res = [[] for _ in range(np.max(y_pred) + 1)]
    for (x, y) in zip(X.values, y_pred):
        res[y].append(x)
    return list(map(np.array, res))


def dist(a, b):
    return np.sqrt((a - b) @ (a - b))


# Среднее внутрикластерное расстояние (Компактность кластеров)
def Cohension(X, y_pred):
    res = 0
    for c in split_clasters(X, y_pred):
        cm = c.mean(axis=0)
        for x in c:
            res += dist(cm, x) ** 2
    return res


# Среднее межкластерное расстояние (Отделимость кластеров)
def Separation(X, y_pred):
    cl = split_clasters(X, y_pred)
    m = X.values.mean(axis=0)
    res = 0
    for c in cl:
        cm = c.mean(axis=0)
        res += dist(cm, m) ** 2
    return len(X) * res


# Внешняя мера
def create_adjacent(X, y, y_pred):
    assert len(y) == len(X) and len(X) == len(y_pred)

    n = np.zeros((np.max(y) + 1, np.max(y_pred) + 1))
    a = np.zeros(np.max(y) + 1)
    b = np.zeros(np.max(y_pred) + 1)
    for (yi, yi_pred) in zip(y, y_pred):
        n[yi][yi_pred] += 1
        a[yi] += 1
        b[yi_pred] += 1

    p = n / len(X)
    pi = a / len(X)
    pj = b / len(X)

    return p, pi, pj


def F(X, y, y_pred):
    p, pi, pj = create_adjacent(X, y, y_pred)
    s = 0
    for j in range(len(pj)):
        m = 0
        for i in range(len(pi)):
            if p[i][j] == 0 or pi[i] == 0 or pj[j] == 0:
                continue
            v = 2 * (p[i][j] / pi[i]) * (p[i][j] / pj[j]) / (p[i][j] / pi[i] + p[i][j] / pj[j])
            if v > m:
                m = v
        s += pj[j] * m
    return s


def find_Clusters(X, X_reduced, to_Drow):
    if to_Drow:
        inertia = []
        F01 = []
        maxK = 15
        for k in range(2, maxK):
            kmeans = KMeans(n_clusters=k, random_state=1).fit(X_reduced)
            inertia.append(np.sqrt(kmeans.inertia_))
            mean_distance_to_center = np.mean([euclidean_distances([kmeans.cluster_centers_[kmeans.labels_[idx]]],
                                                                   [point]) for idx, point in enumerate(X_reduced)])
            mean_center_distance = np.triu(euclidean_distances(kmeans.cluster_centers_)).mean()
            F01.append(mean_distance_to_center / mean_center_distance)

        plt.plot(range(2, maxK), inertia, marker='s')
        plt.xlabel('$k$')
        plt.ylabel('$J(C_k)$')
        plt.show()

        plt.plot(range(2, maxK), F01, marker='s')
        plt.xlabel('$k$')
        plt.ylabel('$F_0/F_1$')
        plt.show()
