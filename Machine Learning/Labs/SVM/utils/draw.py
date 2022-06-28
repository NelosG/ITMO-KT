import numpy as np
from matplotlib import pyplot as plt
from mlxtend.plotting import plot_decision_regions
from sklearn.base import BaseEstimator
from tqdm import tqdm

from utils.SMO import SMOModel


class CLF(BaseEstimator):
    def __init__(self, model: SMOModel):
        self.model = model

    def predict(self, x):
        a = np.zeros(len(x))
        for i in tqdm(range(x.shape[0])):
            if self.model.predict(x[i]) >= 0:
                a[i] = 1
        return a


def draw_plot(X, Y, kernel_function, C):
    model = SMOModel(X, Y, kernel_function, C=C)
    model.train()
    plot_decision_regions(X, Y, clf=CLF(model), legend=2)
    plt.show()


def plot_decision_boundary(model, ax, resolution=100, colors=('b', 'k', 'r'), levels=(-1, 0, 1)):
    xrange = np.linspace(model.X[:, 0].min(), model.X[:, 0].max(), resolution)
    yrange = np.linspace(model.X[:, 1].min(), model.X[:, 1].max(), resolution)
    grid = [[model.predict_row(np.array([xr, yr])) for xr in xrange] for yr in yrange]
    grid = np.array(grid).reshape(len(xrange), len(yrange))

    ax.scatter(model.X[:, 0], model.X[:, 1], c=model.y, cmap=plt.cm.viridis)
    ax.contour(xrange, yrange, grid, levels=levels, linewidths=(1, 1, 1),
               linestyles=('--', '-', '--'), colors=colors)

    mask = np.round(model.alphas, decimals=2) != 0.0
    ax.scatter(model.X[mask, 0], model.X[mask, 1],
               c=model.y[mask], cmap=plt.cm.viridis, lw=1, edgecolors='k')

    return grid, ax


def draw_plot_fast(X, Y, kernel_function, C):
    model = SMOModel(X, Y, kernel_function, C=C)
    model.train()
    fig, ax = plt.subplots()
    plt.title(str(kernel_function))
    _, _ = plot_decision_boundary(model, ax)
    plt.show()


def draw_best_fast(X, Y, besties):
    for val in besties:
        kernel = val[0](val[2])
        print("{}:  C = {}, {}with accuracy: {}".format(
            str(kernel),
            str(val[1]),
            kernel.param_name() + " = " + str(val[2]) + ", " if kernel.param_name() is not None else "",
            str(val[3])
        ))
        draw_plot_fast(X, Y, kernel, val[1])
