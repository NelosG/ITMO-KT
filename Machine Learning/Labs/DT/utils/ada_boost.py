import numpy as np
from sklearn.base import BaseEstimator
from sklearn.tree import DecisionTreeClassifier


class AdaBoost(BaseEstimator):
    def __init__(self, max_depth=2, clf=DecisionTreeClassifier):
        self.M = None
        self.accuracies = None
        self.estimator_weights = None
        self.estimators = None
        self.weights = None
        self.clf = clf
        self.max_depth = max_depth

    def fit(self, X, y, M=55):
        self.weights = np.ones(len(X)) / len(X)
        self.estimators = []
        self.estimator_weights = []
        self.accuracies = []
        self.M = M
        for n in range(self.M):
            tree = self.clf(max_depth=self.max_depth)
            tree.fit(X, y, self.weights)
            self.estimators.append(tree)
            error = np.sum(self.weights[y != tree.predict(X)])
            self.estimator_weights.append(0.5 * np.log((1 - error) / error))
            self.weights *= np.exp(-self.estimator_weights[-1] * y * tree.predict(X))
            self.weights /= np.sum(self.weights)

    def predict(self, X):
        predictions = np.zeros(len(X))
        cur = 0
        for (estimator, weight) in zip(self.estimators, self.estimator_weights):
            if cur >= self.M:
                break
            cur += 1
            predictions += weight * estimator.predict(X)
        return np.sign(predictions)
