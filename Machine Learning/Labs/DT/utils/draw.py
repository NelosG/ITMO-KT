from sklearn.metrics import accuracy_score
import numpy as np
from matplotlib import pyplot as plt
from mlxtend.plotting import plot_decision_regions
from tqdm import tqdm


def draw_plot(X, Y, clf, M=55):
    clf.M = M
    plot_decision_regions(X, Y, clf=clf, legend=2)
    plt.show()


def draw_accuracy(X, Y, clf):
    accuracy = []
    iterations = np.arange(1, 56)
    for i in tqdm(iterations):
        clf.M = i
        predictions = clf.predict(X)
        accuracy.append(accuracy_score(Y, predictions))

    fig, ax = plt.subplots()
    ax.plot(iterations, accuracy, linewidth=2.0)
    plt.show()


def plot_decision_boundary(X, y, clf, ax, resolution=100):
    xrange = np.linspace(X[:, 0].min(), X[:, 0].max(), resolution)
    yrange = np.linspace(X[:, 1].min(), X[:, 1].max(), resolution)
    grid = [[clf.predict([np.array([xr, yr])]) for xr in xrange] for yr in yrange]
    grid = np.array(grid).reshape(len(xrange), len(yrange))

    ax.scatter(X[:, 0], X[:, 1], c=y, cmap=plt.cm.viridis)
    ax.contour(xrange, yrange, grid, linewidths=0.5,
               linestyles='-', colors='b')
    return grid, ax


def draw_plot_fast(X, Y, clf):
    fig, ax = plt.subplots()
    _, _ = plot_decision_boundary(X, Y, clf, ax)
    plt.show()


def draw_all_ada(X, Y, clf, Ms=None):
    if Ms is None:
        Ms = [1, 2, 3, 5, 8, 13, 21, 34, 55]
    for M in Ms:
        clf.M = M
        draw_plot_fast(X, Y, clf)
