from typing import Any

from pandas import DataFrame
from sklearn.ensemble import RandomForestClassifier
from sklearn.metrics import make_scorer, accuracy_score, fbeta_score
from sklearn.model_selection import train_test_split, GridSearchCV


def opt_and_print(features_final: DataFrame, label: Any) -> Any:
    # Split the 'features' and 'income' Data into training and testing sets
    X_train, X_test, y_train, y_test = train_test_split(features_final,
                                                        label,
                                                        test_size=0.2,
                                                        random_state=0)

    # TODO: Initialize the classifier
    clf = RandomForestClassifier()
    clf2 = RandomForestClassifier()

    clf2.fit(X_train, y_train)

    # TODO: Create the parameters list you wish to tune, using a dictionary if needed.
    # HINT: parameters = {'parameter_1': [value1, value2], 'parameter_2': [value1, value2]}
    parameters = {'n_estimators': [5, 10, 100],
                  'min_samples_split': [2, 10, 50],
                  'max_features': ["sqrt", "log2"],
                  }

    # TODO: Make an fbeta_score scoring object using make_scorer()
    scorer = make_scorer(fbeta_score, beta=1)

    # TODO: Perform grid search on the classifier using 'scorer' as the scoring method using GridSearchCV()
    grid_obj = GridSearchCV(clf, param_grid=parameters, scoring=scorer)

    # TODO: Fit the grid search object to the training Data and find the optimal parameters using fit()
    grid_fit = grid_obj.fit(X_train, y_train)

    # Get the estimator
    best_clf = grid_fit.best_estimator_

    # Make predictions using the unoptimized and model
    predictions = (clf.fit(X_train, y_train)).predict(X_test)
    best_predictions = best_clf.predict(X_test)

    # Report the before-and-afterscores
    print("Unoptimized model\n------")
    print("Accuracy score on testing Data: {:.4f}".format(accuracy_score(y_test, predictions)))
    print("F-score on testing Data: {:.4f}".format(fbeta_score(y_test, predictions, beta=1)))
    print("\nOptimized Model\n------")
    print("Final accuracy score on the testing Data: {:.4f}".format(accuracy_score(y_test, best_predictions)))
    print("Final F-score on the testing Data: {:.4f}".format(fbeta_score(y_test, best_predictions, beta=1)))
    return best_clf
