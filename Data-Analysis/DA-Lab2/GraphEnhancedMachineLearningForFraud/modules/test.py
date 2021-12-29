import numpy as np
import scipy.stats


def mean_confidence_interval(data: list, confidence: float = 0.95):
    a = 1.0 * np.array(data)
    n = len(a)
    m, se = np.mean(a), scipy.stats.sem(a)
    h = se * scipy.stats.t._ppf((1 + confidence) / 2., n - 1)
    return m, m - h, m + h


def run_test(training_results_lists: dict[str, list]):
    tests = ['f1', 'TP', 'TN', 'FP', 'FN']

    for test in tests:
        mean, lower, upper = mean_confidence_interval(
            training_results_lists['{}_enh'.format(test)] - training_results_lists['{}_std'.format(test)],
            confidence=0.99)
        print("Test {},  lower: {}, mean:{} upper: {}".format(test, lower, mean, upper))
