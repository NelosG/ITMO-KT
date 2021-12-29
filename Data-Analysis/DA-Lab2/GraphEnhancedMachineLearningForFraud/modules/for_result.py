from typing import Any

from pandas import DataFrame
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import fbeta_score
from sklearn.model_selection import train_test_split
import time


def calc_metrics(predictions: Any, true_values: Any) -> dict[str, int]:
    TP = 0
    FP = 0
    TN = 0
    FN = 0

    for i, true_val in enumerate(true_values):
        if true_val == predictions[i] == 1:
            TP += 1
        if predictions[i] == 1 and true_val != predictions[i]:
            FP += 1
        if true_val == predictions[i] == 0:
            TN += 1
        if predictions[i] == 0 and true_val != predictions[i]:
            FN += 1
    return {'TP': TP, 'TN': TN, 'FP': FP, 'FN': FN}


# Function for training and recording the results of both models on a specified random seed.
def train_models(seed: int, features_final: DataFrame, features_enhanced: DataFrame, label: Any) -> dict[str, Any]:
    X_train_std, X_test_std, y_train, y_test = train_test_split(features_final,
                                                                label,
                                                                test_size=0.2,
                                                                random_state=seed)

    X_train_enh, X_test_enh, y_train_enh, y_test_enh = train_test_split(features_enhanced,
                                                                        label,
                                                                        test_size=0.2,
                                                                        random_state=seed)

    # TODO: Initialize the classifier
    clf_std = RandomForestClassifier(max_features='sqrt', min_samples_split=50, n_estimators=100)
    clf_enh = RandomForestClassifier(max_features='sqrt', min_samples_split=50, n_estimators=100)

    t0 = time.time()
    clf_std.fit(X_train_std, y_train)
    training_time_std = time.time() - t0

    t1 = time.time()
    clf_enh.fit(X_train_enh, y_train)
    training_time_enh = time.time() - t1

    t2 = time.time()
    predictions_std = clf_std.predict(X_test_std)
    pred_time_std = time.time() - t2

    t3 = time.time()
    predictions_enh = clf_enh.predict(X_test_enh)
    pred_time_enh = time.time() - t3

    f1_std = fbeta_score(y_test, predictions_std, beta=1)
    f1_enh = fbeta_score(y_test, predictions_enh, beta=1)
    return {'met_std': calc_metrics(predictions_std, y_test), 'met_enh': calc_metrics(predictions_enh, y_test),
            'f1_std': f1_std, 'f1_enh': f1_enh, 'times': {'training_time_std': training_time_std,
                                                          'training_time_enh': training_time_enh,
                                                          'pred_time_std': pred_time_std,
                                                          'pred_time_enh': pred_time_enh}}


def print_metric_comparison(training_seeds_result:  list[dict[str, Any]]):
    TP_sum_std = 0
    FP_sum_std = 0
    TN_sum_std = 0
    FN_sum_std = 0
    TP_sum_enh = 0
    FP_sum_enh = 0
    TN_sum_enh = 0
    FN_sum_enh = 0
    f1_enh_sum = 0
    f1_std_sum = 0

    training_time_std_avg = 0
    training_time_enh_avg = 0
    pred_time_std_sum_avg = 0
    pred_time_enh_sum_avg = 0

    for o in training_seeds_result:
        TP_sum_std += o['met_std']['TP']
        FP_sum_std += o['met_std']['FP']
        TN_sum_std += o['met_std']['TN']
        FN_sum_std += o['met_std']['FN']
        TP_sum_enh += o['met_enh']['TP']
        FP_sum_enh += o['met_enh']['FP']
        TN_sum_enh += o['met_enh']['TN']
        FN_sum_enh += o['met_enh']['FN']
        f1_enh_sum += o['f1_enh']
        f1_std_sum += o['f1_std']

        training_time_std_avg += o['times']['training_time_std'] / 100
        training_time_enh_avg += o['times']['training_time_enh'] / 100
        pred_time_std_sum_avg += o['times']['pred_time_std'] / 100
        pred_time_enh_sum_avg += o['times']['pred_time_enh'] / 100
    print("---------- Metric comparison ----------")
    print("TP_sum_std: {}".format(TP_sum_std / 100.0))
    print("FP_sum_std: {}".format(FP_sum_std / 100.0))
    print("TN_sum_std: {}".format(TN_sum_std / 100.0))
    print("FN_sum_std: {}".format(FN_sum_std / 100.0))
    print("TP_sum_enh: {}".format(TP_sum_enh / 100.0))
    print("FP_sum_enh: {}".format(FP_sum_enh / 100.0))
    print("TN_sum_enh: {}".format(TN_sum_enh / 100.0))
    print("FN_sum_enh: {}".format(FN_sum_enh / 100.0))
    print("f1_enh_sum: {}".format(f1_enh_sum / 100.0))
    print("f1_std_sum: {}".format(f1_std_sum / 100.0))
    print("---------- Time Evaluation ----------")
    print('training_time_std: {}'.format(training_time_std_avg))
    print('training_time_enh {}:'.format(training_time_enh_avg))
    print('pred_time_std {}:'.format(pred_time_std_sum_avg))
    print('pred_time_enh {}:'.format(pred_time_enh_sum_avg))


def statistic_significance(training_seeds_result: list[dict[str, Any]]) -> \
        dict[str, list]:
    training_results_lists = {
        'TP_std': [],
        'FP_std': [],
        'TN_std': [],
        'FN_std': [],
        'TP_enh': [],
        'FP_enh': [],
        'TN_enh': [],
        'FN_enh': [],
        'f1_enh': [],
        'f1_std': []
    }

    for o in training_seeds_result:
        training_results_lists['TP_std'].append(o['met_std']['TP'])
        training_results_lists['FP_std'].append(o['met_std']['FP'])
        training_results_lists['TN_std'].append(o['met_std']['TN'])
        training_results_lists['FN_std'].append(o['met_std']['FN'])
        training_results_lists['TP_enh'].append(o['met_enh']['TP'])
        training_results_lists['FP_enh'].append(o['met_enh']['FP'])
        training_results_lists['TN_enh'].append(o['met_enh']['TN'])
        training_results_lists['FN_enh'].append(o['met_enh']['FN'])
        training_results_lists['f1_enh'].append(o['f1_enh'])
        training_results_lists['f1_std'].append(o['f1_std'])
    return training_results_lists
