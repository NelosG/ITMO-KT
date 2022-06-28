import numpy as np
from numpy import ndarray


def sum_of_squares(real: np.ndarray, predict: np.ndarray) -> ndarray:
    return np.sum((real - predict) ** 2)


def MSE(real: np.ndarray, predict: np.ndarray) -> float:
    return sum_of_squares(real, predict) / len(real)


def MSE_gradient(real, predict, x):
    gradient = []
    for i in range(len(x)):
        gradient.append(2 * (predict - real) * x[i])
    return np.array(gradient)


def SMAPE(real: np.ndarray, predict: np.ndarray) -> float:
    res = 0
    for i in range(len(real)):
        if (abs(real[i]) + abs(predict[i])) != 0:
            res += abs(real[i] - predict[i]) / (abs(real[i]) + abs(predict[i]))
    return res / len(real)


def SMAPE_gradient(real, predict, x):
    gradient = []
    scalar = predict
    for i in range(len(x)):
        numerator = abs(scalar - real)
        denominator = abs(scalar) + abs(real)
        module1 = x[i] * numerator / (scalar - real)
        module2 = x[i] * scalar / abs(scalar)
        gradient.append((module1 * denominator - numerator * module2) / (denominator * denominator))
    return np.array(gradient)


def RMSE(real: np.ndarray, predict: np.ndarray) -> float:
    return MSE(real, predict) ** 0.5


def NRMSE(real: np.ndarray, predict: np.ndarray) -> float:
    return RMSE(real, predict) / (max(real) - min(real))


def apply_error_function(X, Y_test, w, function):
    Y_pred = (X @ w.T).T
    return function(Y_test, Y_pred)
