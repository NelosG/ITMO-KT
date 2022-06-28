import numpy as np
from joblib import Parallel, delayed
import math
from sklearn.base import BaseEstimator

from utils.decision_tree import DecisionTree


def most_frequent_elem(elems):
    groups, counts = np.unique(elems, return_counts=True)
    return groups[np.argmax(counts)]


class DecisionForest(BaseEstimator):
    def __init__(self, trees_cnt=10):
        self.trees = None
        self.trees_features = None
        self.trees_cnt = trees_cnt
        self.features_per_tree = lambda x: math.ceil(np.sqrt(x))

    def train_tree(self, idx, X, y):
        cnt, dim = X.shape
        perm = np.random.choice(cnt, cnt, replace=True)
        tree = DecisionTree(np.inf)
        tree.fit(X[perm][:, self.trees_features[idx]], y[perm])
        return tree

    def fit(self, X, y):
        cnt, dim = X.shape
        tree_features = self.features_per_tree(dim)
        self.trees_features = [np.random.choice(dim, tree_features, replace=False) for _ in range(self.trees_cnt)]
        self.trees = Parallel(n_jobs=-1)(delayed(self.train_tree)(i, X, y)
                                         for i in range(self.trees_cnt))

    def predict_one(self, x):
        decisions = np.zeros(self.trees_cnt)
        for i in range(self.trees_cnt):
            decisions[i] = self.trees[i].predict_one(x[self.trees_features[i]])
        return most_frequent_elem(decisions)

    def predict(self, X):
        return np.apply_along_axis(self.predict_one, 1, X)
