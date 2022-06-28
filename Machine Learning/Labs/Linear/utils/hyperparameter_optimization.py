from utils.algorithms import stochastic_gradient_descent, least_squares_approximation
from utils.error_function import apply_error_function, SMAPE


def objective_sgd(trial, X_train, Y_train, X_test, Y_test, function, gradient):
    tau = trial.suggest_uniform("tau", 1e-5, 20)
    alpha = trial.suggest_uniform("alpha", 1e-5, 1)
    step = trial.suggest_uniform("step", 1e-10, 3)

    fit, _ = stochastic_gradient_descent(X_train, Y_train, function, gradient, tau, alpha, step, 2000)

    error = apply_error_function(X_test, Y_test, fit, SMAPE)

    return error


def objective_lsa(trial, X_train, Y_train, X_test, Y_test, function):
    tau = trial.suggest_uniform("tau", 1e-10, 1e-3)

    fit = least_squares_approximation(X_train, Y_train, tau)

    error = apply_error_function(X_test, Y_test, fit, function)

    return error
