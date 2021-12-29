import numpy as np
import yaml
from numpy import ndarray
from typing import *

from modules import read

# Some needed stuff
from sklearn.feature_extraction.text import CountVectorizer
from sklearn.feature_extraction.text import TfidfTransformer
from sklearn.model_selection import train_test_split
import datetime
import matplotlib.pyplot as plt
from sklearn.model_selection import KFold, cross_val_score

# Classifiers
from sklearn.naive_bayes import MultinomialNB
from sklearn.linear_model import LogisticRegression
from sklearn.tree import DecisionTreeClassifier
from sklearn.neighbors import KNeighborsClassifier
from sklearn.svm import SVC


def reload_config(path: str) -> dict[str, Any]:
    with open(path) as stream:
        config_cur = yaml.safe_load(stream)
    return config_cur


def plot_accuracy(rng: Iterable[Any], accuracy: list[Union[int, float]], best_k: Union[int, float],
                  best_score: Union[int, float], args: str = None) -> None:
    if args is not None:
        plt.plot(rng, accuracy, args)
    else:
        plt.plot(rng, accuracy)
    plt.show()
    print("Best kernel = ", best_k)
    print("Best score = ", best_score)


class Worker:
    def __init__(self, seed: int, columns: list[str], xtr: ndarray, xtest: ndarray, ytr: ndarray, ytest: ndarray,
                 rng: Iterable[Any], supplier: Callable[[int], Any]):
        self.seed = seed
        self.columns = columns

        self.X_train = xtr
        self.X_test = xtest
        self.y_train = ytr
        self.y_test = ytest
        self.rng = rng
        self.supplier = supplier

    def calc_accuracy(self) -> tuple[list[Union[int, float]], Union[int, float], Union[int, float]]:
        accuracy = []
        best_k = 1
        best_score = 0
        for k in self.rng:
            classifier = self.supplier(k)
            kf = KFold(len(self.y_train), random_state=self.seed, shuffle=True)
            scores = cross_val_score(classifier, self.X_train, self.y_train, cv=kf)
            if scores.mean() > best_score:
                best_score = scores.mean()
                best_k = k
            accuracy.append(scores.mean())
        return accuracy, best_k, best_score

    def calc_and_print(self, best_k: Union[int, float], name: str = None, ) -> None:
        y_test = self.y_test
        start_time = datetime.datetime.now()
        classifier = self.supplier(best_k)
        classifier.fit(self.X_train, self.y_train)
        y_test_predict = classifier.predict(self.X_test)
        fp = 0
        tn = 0
        fn = 0
        tp = 0
        f = 0
        t = 0
        for i in range(len(y_test)):
            if y_test[i] == 0 and y_test_predict[i] == 1:
                fp += 1
                f += 1
            if y_test[i] == 0 and y_test_predict[i] == 0:
                fn += 1
                f += 1
            else:
                if y_test[i] == 1 and y_test_predict[i] == 0:
                    tn += 1
                    t += 1
                if y_test[i] == 1 and y_test_predict[i] == 1:
                    tp += 1
                    t += 1

        params = (name, float(fp) / len(y_test), float(tn) / len(y_test), float(fn) / f, float(tp) / t,
                  datetime.datetime.now() - start_time)
        for a, b in zip(self.columns, params):
            print(a, "\t = ", b)

    def all_in_one(self, name: str = None) -> None:
        accuracy, best_k, best_score = self.calc_accuracy()
        plot_accuracy(self.rng, accuracy, best_k, best_score)
        self.calc_and_print(best_k, name)
