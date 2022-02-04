from typing import Any
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
from pandas import DataFrame
from sklearn.ensemble import RandomForestClassifier


def draw_func(clf_enh: RandomForestClassifier, features_enhanced: DataFrame, X_train_enh: Any):
    matplotlib.rcParams.update({'font.size': 22})
    importances = clf_enh.feature_importances_
    indices = np.argsort(importances)[::-1]

    plt.figure(figsize=(24, 5))
    plt.xlabel("Feature Name", fontsize=30)
    plt.ylabel("Importance ", fontsize=30)
    plt.bar(range(5), importances[indices[0:5]],
            color="r", align="center")
    plt.xticks(range(5), features_enhanced.columns[indices[0:5]].get_values())
    plt.xlim([-1, 5])
    plt.show()

    plt.figure(figsize=(24, 5))
    plt.xlabel("Feature", fontsize=30)
    plt.ylabel("Importance", fontsize=30)
    plt.plot(range(X_train_enh.shape[1]), importances[indices],
             color="r")

    plt.xlim([-1, X_train_enh.shape[1]])
    plt.show()
