import random
from decimal import Decimal

import numpy as np

epsilon = 1e-7


def init_weights(count_objects: int, count_features: int) -> np.ndarray:
    w = np.zeros(count_features)
    random.seed(0)
    for i in range(count_features):
        w[i] = random.uniform(-0.5 / count_objects, 0.5 / count_objects)
    return w


def init_l(X, Y, w, function):
    Q = 0
    for i in range(len(X)):
        Q += function(np.array([X[i].dot(w)]), np.array([Y[i]]))
    return Q / len(X)


def stochastic_gradient_descent(X: np.ndarray, Y: np.ndarray, error, grad, tau, alpha, init_step,
                                iterations_limit):
    random.seed(0)
    n = len(X)
    count_objects = len(X)
    count_features = len(X[0])
    w = init_weights(count_objects, count_features)
    L = Decimal(init_l(X, Y, w, error))
    l_taus = []
    for i in range(iterations_limit):
        mu = init_step / (i + 1)
        j = random.randint(0, n - 1)
        wx = np.array([w.dot(X[j])])
        l_tau = error(np.array([Y[j]]), wx) + (tau / 2) * (np.linalg.norm(w) ** 2)
        l_taus.append(l_tau)
        L_prev = L
        L = Decimal(1 - alpha) * Decimal(L) + Decimal(alpha) * Decimal(l_tau)
        w_new = w * (1.0 - tau * mu) - mu * grad(Y[j], wx[0], X[j])
        if L == Decimal('inf') or L == Decimal('-inf'):
            L = L_prev
        elif np.dot(w_new - w, w_new - w) < epsilon or abs(L - L_prev) < epsilon:
            return w, l_taus
        w = w_new
    return w, l_taus


def least_squares_approximation(X: np.ndarray, Y: np.ndarray, tau: float) -> np.ndarray:
    _, size = X.shape
    return np.linalg.inv(X.T @ X + tau * np.identity(size)) @ X.T @ Y
