import numpy as np
import pandas as pd
from sklearn.preprocessing import MinMaxScaler


def read_data(filename: str):
    dataset = pd.read_csv(filename)
    xs = dataset.iloc[:, 0:2].to_numpy()

    xs = MinMaxScaler().fit_transform(xs)
    ys = dataset.iloc[:, 2:3].to_numpy()
    ys = np.concatenate(ys).ravel()
    ys = np.array(list(map(lambda x: 1 if x == 'P' else -1, ys)))

    indices = np.arange(ys.shape[0])
    np.random.shuffle(indices)
    return xs[indices], ys[indices]