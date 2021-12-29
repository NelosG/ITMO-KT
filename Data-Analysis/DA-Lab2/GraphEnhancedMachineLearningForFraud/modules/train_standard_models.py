from typing import Any, Union
from pandas import DataFrame
from sklearn.model_selection import *
from sklearn.svm import SVC
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import fbeta_score, accuracy_score


def some_func(features_final: DataFrame, label: Any) -> tuple[list[dict[str, Any]], list[dict[str, float]]]:
    kf = KFold(n_splits=5)
    fold_betas = []
    fold_accuracy = []
    kf.get_n_splits(features_final)

    for train_index, test_index in kf.split(features_final):
        X_train, X_test = features_final.iloc[train_index], features_final.iloc[test_index]
        y_train, y_test = label.iloc[train_index], label.iloc[test_index]

        clfSVM = SVC()
        clfSVM.fit(X_train, y_train)
        clfRandomForest = RandomForestClassifier()
        clfRandomForest.fit(X_train, y_train)

        predictionsSVM = clfSVM.predict(X_test)
        predictionsRF = clfRandomForest.predict(X_test)

        fold_betas.append({"SVM": fbeta_score(y_test, predictionsSVM, average='macro', beta=1),
                           "RF": fbeta_score(y_test, predictionsRF, average='macro', beta=1)})
        fold_accuracy.append({"SVM": accuracy_score(y_test, predictionsSVM),
                              "RF": accuracy_score(y_test, predictionsRF)})
        return fold_betas, fold_accuracy


def other_func(features_final: DataFrame, features_enhanced: DataFrame, label: Any) -> tuple[
    list[dict[str, Any]], list[dict[str, float]]]:
    kf = KFold(n_splits=5)
    fold_betas = []
    fold_accuracy = []
    kf.get_n_splits(features_final)

    for train_index, test_index in kf.split(features_final):
        X_train_final, X_test_final = features_final.iloc[train_index], features_final.iloc[test_index]
        X_train_enh, X_test_enh = features_enhanced.iloc[train_index], features_enhanced.iloc[test_index]
        y_train, y_test = label.iloc[train_index], label.iloc[test_index]

        clf_final = RandomForestClassifier(max_features='sqrt', min_samples_split=50, n_estimators=100)
        clf_enh = RandomForestClassifier(max_features='sqrt', min_samples_split=50, n_estimators=100)

        clf_final.fit(X_train_final, y_train)
        clf_enh.fit(X_train_enh, y_train)

        predictions_final = clf_final.predict(X_test_final)
        predictions_enh = clf_enh.predict(X_test_enh)

        fold_betas.append({"standard": fbeta_score(y_test, predictions_final, average='macro', beta=1),
                           "enhanced": fbeta_score(y_test, predictions_enh, average='macro', beta=1)})
        fold_accuracy.append({"standard": accuracy_score(y_test, predictions_final),
                              "enhanced": accuracy_score(y_test, predictions_enh)})
        return fold_betas, fold_accuracy


def calculate_average_for_fold(fold_data: list[Union[dict[str, Any], dict[str, Any]]], model_names: list[str]):
    model_1_sum = 0
    model_2_sum = 0

    for dat in fold_data:
        model_1_sum += dat[model_names[0]] / len(fold_data)
        model_2_sum += dat[model_names[1]] / len(fold_data)

    print("Model {} average: {}".format(model_names[0], model_1_sum))
    print("Model {} average: {}".format(model_names[1], model_2_sum))
