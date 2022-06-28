import random

import numpy as np

eps = 1.0e-9
tol = 1.0e-9
max_iter = 50000
passes_limit = 100


class SMOModel:
    def __init__(self, X, y, kernel, C, kernel_matrix=None):
        self.X = X
        self.y = y
        self.C = C
        self.kernel = kernel
        self.m = len(self.X)
        self.alphas = np.zeros(self.m)
        self.b = 0.0
        self._obj = []
        if kernel_matrix is None:
            self.kernel_matrix = [[np.nan] * self.m for _ in range(self.m)]
        else:
            self.kernel_matrix = kernel_matrix

    def get_kernel(self, i, j):
        if np.isnan(self.kernel_matrix[i][j]):
            if not np.isnan(self.kernel_matrix[j][i]):
                return self.kernel_matrix[j][i]
            self.kernel_matrix[i][j] = self.kernel(self.X[i], self.X[j])
        return self.kernel_matrix[i][j]

    def take_step(self, i, j):
        error_i = self.__calculate_error(i)

        if ((self.y[i] * error_i < -tol) & (self.alphas[i] < self.C)) | (
                (self.y[i] * error_i > tol) & (self.alphas[i] > 0)):
            eta = self.__calculate_eta(i, j)

            if eta >= 0:
                return 0

            L, H = self.__find_alpha_bounds(i, j)
            if abs(L - H) < eps:
                return 0

            error_j = self.__calculate_error(j)

            alpha_old_i = self.alphas[i]
            alpha_old_j = self.alphas[j]

            self.alphas[j] -= self.y[j] * float(error_i - error_j) / eta
            self.alphas[j] = max(self.alphas[j], L)
            self.alphas[j] = min(self.alphas[j], H)

            if abs(self.alphas[j] - alpha_old_j) < eps:
                return 0
            self.alphas[i] += self.y[i] * self.y[j] * (alpha_old_j - self.alphas[j])

            supp_Y_i = self.y[i] * (self.alphas[i] - alpha_old_i)
            supp_Y_j = self.y[j] * (self.alphas[j] - alpha_old_j)

            b1 = (
                    self.b - error_i
                    - supp_Y_i * self.get_kernel(i, i)
                    - supp_Y_j * self.get_kernel(i, j)
            )
            b2 = (
                    self.b - error_j
                    - supp_Y_i * self.get_kernel(i, j)
                    - supp_Y_j * self.get_kernel(j, j)
            )

            if 0 < self.alphas[i] < self.C:
                self.b = b1
            elif 0 < self.alphas[j] < self.C:
                self.b = b2
            else:
                self.b = (b1 + b2) / 2
            return 1
        return 0

    def train(self):
        iters = 0
        cur_passes = 0
        while iters < max_iter and (cur_passes < passes_limit):
            alpha_prev = np.copy(self.alphas)
            num_changes = 0
            for i in range(self.m):
                iters += 1
                j = self.__random_index(i)
                self.take_step(i, j)
            cur_passes += num_changes == 0
            diff = np.linalg.norm(self.alphas - alpha_prev)
            if diff < tol:
                break

    def predict_row(self, X, index=None):
        res = 0
        for i in range(len(self.y)):
            if index is None:
                kv = self.kernel(self.X[i], X)
            else:
                kv = self.get_kernel(i, index)
            res += self.alphas[i] * self.y[i] * kv
        return self.b + res

    def predict(self, X):
        return np.sign(self.predict_row(X))

    def __random_index(self, z):
        i = random.randrange(0, self.m)
        while i == z:
            i = random.randrange(0, self.m)
        return i

    def __find_alpha_bounds(self, i, j):
        if self.y[i] != self.y[j]:
            L = max(0, self.alphas[j] - self.alphas[i])
            H = min(self.C, self.C + self.alphas[j] - self.alphas[i])
        else:
            L = max(0, self.alphas[i] + self.alphas[j] - self.C)
            H = min(self.C, self.alphas[i] + self.alphas[j])
        return L, H

    def __calculate_eta(self, i, j):
        return 2.0 * self.get_kernel(i, j) - self.get_kernel(i, i) - self.get_kernel(j, j)

    def __calculate_error(self, i):
        return self.predict_row(self.X[i], i) - self.y[i]
