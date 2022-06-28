from numpy import sqrt


# DISTANCE FUNCTIONS

def manhattan_d(row1, row2):
    distance = 0.0
    for i, j in zip(row1, row2):
        distance += abs(i - j)
    return distance


def euclidean_d(row1, row2):
    distance = 0.0
    for i, j in zip(row1, row2):
        distance += (i - j) ** 2
    return sqrt(distance)


def chebyshev_d(row1, row2):
    distance = 0.0
    for i, j in zip(row1, row2):
        distance = max(distance, abs(i - j))
    return distance
