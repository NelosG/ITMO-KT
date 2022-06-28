import numpy as np
from numpy.typing import NDArray
from sklearn.base import BaseEstimator
from wquantiles import median


def gini(y: NDArray, weights: NDArray):
    res = 0
    for c in np.unique(y):
        res += (weights[y == c]).sum() ** 2
    return 1 - res / np.sum(weights) ** 2


def split_data(X: NDArray, y: NDArray, weights: NDArray, feature: int, threshold: float):
    left_X = X[X[:, feature] <= threshold]
    left_y = y[X[:, feature] <= threshold]
    left_weights = weights[X[:, feature] <= threshold]
    right_X = X[X[:, feature] > threshold]
    right_y = y[X[:, feature] > threshold]
    right_weights = weights[X[:, feature] > threshold]
    return left_X, left_y, left_weights, right_X, right_y, right_weights


def find_best_split(X: NDArray, y: NDArray, weights: NDArray):
    best_feature = None
    best_threshold = None
    best_score = -np.inf
    for feature in range(X.shape[1]):
        thresholds = np.unique(X[:, feature])
        for threshold in thresholds:
            left_X, left_y, left_weights, right_X, right_y, right_weights = split_data(X, y, weights, feature,
                                                                                       threshold)
            if len(left_y) == 0 or len(right_y) == 0:
                continue
            score = gini(y, weights) - (
                    gini(left_y, left_weights) * np.sum(left_weights) +
                    gini(right_y, right_weights) * np.sum(right_weights)) / np.sum(weights)
            if score > best_score:
                best_feature = feature
                best_threshold = threshold
                best_score = score
    return best_feature, best_threshold


class DecisionTree(BaseEstimator):
    class Node:
        def __init__(self, value, left=None, right=None, feature=None, threshold=None):
            self.value = value
            self.left = left
            self.right = right
            self.feature = feature
            self.threshold = threshold

        def do_predict(self, x: NDArray):
            if self.value is None:
                if x[self.feature] <= self.threshold:
                    return self.left.do_predict(x)
                else:
                    return self.right.do_predict(x)
            return self.value

    def __init__(self, max_depth=None):
        self.root = None
        self.max_depth = max_depth

    def fit(self, X: NDArray, y: NDArray, weights: NDArray = None):
        if weights is None:
            weights = np.ones(len(y)) / len(y)
        self.root = self.build_tree(X, y, weights)

    def build_tree(self, X: NDArray, y: NDArray, weights: NDArray, depth: int = 0):
        if self.max_depth is not None and depth > self.max_depth:
            return self.Node(median(y, weights))
        if len(np.unique(y)) == 1:
            return self.Node(y[0])
        feature, threshold = find_best_split(X, y, weights)
        if feature is None:
            return self.Node(np.mean(y))
        left_X, left_y, left_weights, right_X, right_y, right_weights = split_data(X, y, weights, feature,
                                                                                   threshold)
        left = self.build_tree(left_X, left_y, left_weights, depth + 1)
        right = self.build_tree(right_X, right_y, right_weights, depth + 1)
        return self.Node(None, left, right, feature, threshold)

    def predict(self, X: NDArray):
        return np.apply_along_axis(self.predict_one, 1, X)

    def predict_one(self, x: NDArray):
        return self.root.do_predict(x)
