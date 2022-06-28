from typing import Tuple, List, TextIO

import numpy as np
from matplotlib import pyplot as plt
from utils.algorithms import stochastic_gradient_descent


def read_some(file: TextIO) -> Tuple[List[np.ndarray], List[float]]:
    X = []
    Y = []
    count_objects = int(file.readline())
    for i in range(count_objects):
        features = [float(number) for number in file.readline().split(' ')]
        target = features.pop()
        X.append(np.array(features))
        Y.append(target)
    return X, Y


def load_file(path: str) -> Tuple[List[np.ndarray], List[float], List[np.ndarray], List[float]]:
    with open(path) as file:
        count_features = int(file.readline())
        X_train, Y_train = read_some(file)
        X_test, Y_test = read_some(file)
        return X_train, Y_train, X_test, Y_test


def calculate_ema(values, alpha):
    ema = [0]
    for elem in values:
        ema.append((elem * alpha) + ema[-1] * (1 - alpha))
    return ema


# For debug
def graf_l_taus(X_train, Y_train, tau, alpha, step, gradientFun, delta):
    w, l_taus = stochastic_gradient_descent(X_train, Y_train, delta, gradientFun, tau, alpha, step,  2000)

    ema_X = list(range(len(l_taus) + 1))
    ema_Y = calculate_ema(l_taus, 0.5)

    plt.xlabel('Number of objects')
    plt.ylabel('Error')
    plt.plot(ema_X, ema_Y, label='EMA')
    plt.legend()
    plt.show()
