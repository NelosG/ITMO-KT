import itertools
import multiprocessing

from sklearn.model_selection import KFold

from utils.SMO import SMOModel


def test_kernel(X, Y, Cs, kernel_function, kernel_params=None, multi=True):
    if kernel_params is None:
        kernel_params = [0]

    if multi:
        count = multiprocessing.cpu_count() - 2
        count = 1 if count <= 0 else count
        pool = multiprocessing.Pool(count)
        temp = pool.starmap(for_run, itertools.product([[X, Y, kernel_function]], kernel_params, Cs))
        pool.close()
        pool.join()
    else:
        temp = map(for_run, itertools.product([[X, Y, kernel_function]], kernel_params, Cs))

    results = temp
    return results


def for_run(setup, kernel_param, C):
    X, Y, kernel_function = setup
    accuracies = 0
    total = 0
    kf = KFold(n_splits=3, shuffle=True)
    for train_index, test_index in kf.split(Y):
        X_train = X[train_index]
        Y_train = Y[train_index]
        X_test = X[test_index]
        Y_test = Y[test_index]
        model = SMOModel(X_train, Y_train, kernel_function(kernel_param), C)
        model.train()
        accuracies += calc_accuracy(X_test, Y_test, model)
        total += 1
    avg = accuracies / total
    return [avg, C, kernel_param]


def calc_accuracy(X_test, Y_test, model: SMOModel):
    true = 0
    for i in range(len(Y_test)):
        true += (model.predict(X_test[i]) == Y_test[i])
    return true / len(Y_test)
