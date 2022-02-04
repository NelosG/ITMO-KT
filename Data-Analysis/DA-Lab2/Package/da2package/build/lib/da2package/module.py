from typing import List, Tuple, Union
from collections import Counter
import numpy as np
from matplotlib import pyplot as plt

def  print_hello():
    print("Hello, DA")

def fast_hist(array: List[Union[int, float]],
              bins_count: int) -> Tuple[List[int], List[float]]:
    """
    Builds bins' labels and bins' value counts for given array
    :param array: array with numeric values
    :param bins_count:  number of bins in result distribution
    :return: Two lists:
             first contains value counts of each bin,
             second contains list of bins' labels
    """

    dict_unique = Counter(array)
    unique = sorted(dict_unique)
    min_elem = min(array)
    max_elem = max(array)

    step = (max_elem - min_elem) / bins_count
    upper = min_elem + step * bins_count + step * 0.9
    bins = np.arange(min_elem, upper, step)

    # я нашел функцию для этого в нумпае, но думаю что не стоит ее использовать (к тому же я не знаю как именно она работает)
    # bins = np.histogram_bin_edges(array, bins_count, (min_elem, max_elem))
    value_counts = [0] * (len(bins) -1)
    index_unique = 0
    index_labels = 0
    while True:
        if index_labels >= len(bins):
            break
        if index_unique >= len(unique):
            break

        if (bins[index_labels] <= unique[index_unique]) and (index_labels + 1 >= len(bins) or (unique[index_unique] < bins[index_labels + 1])):
            value_counts[(index_labels if index_labels < (len(bins) - 1) else (index_labels - 1))] += dict_unique[unique[index_unique]]
            index_unique += 1
        else:
            index_labels += 1

    return value_counts, bins


def draw(array):
    value_counts_my, bins_names = fast_hist(array, len(array))
    def generate_labels(bins_names):
        bins_max_ind = len(bins_names) - 1
        labels_pos = []
        labels = []
        labels_pos.append(0.)
        labels.append(0)
        for i in range(1, 6):
            labels_pos.append(round(bins_max_ind / 5 * i))
            labels.append(round(bins_names[labels_pos[i]], 1))

        labels_pos.append(bins_max_ind)
        labels.append(round(bins_names[bins_max_ind], 1))
        return labels_pos, labels

    labels_pos, labels = generate_labels(bins_names)
    x_pos = np.arange(len(value_counts_my))
    plt.bar(x_pos, value_counts_my)
    plt.xticks(labels_pos, labels)
    plt.show()
    print('Значения колонок:', value_counts_my)
    print('Названия колонок:', bins_names)
