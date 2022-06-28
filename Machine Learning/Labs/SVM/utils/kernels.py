import numpy as np


class Kernel(object):
    def __init__(self, name):
        self.name = name

    def __str__(self):
        return self.name

    def param_name(self):
        return None


class PolynomialKernel(Kernel):
    def __init__(self, degree=2):
        super().__init__("Polynomial kernel")
        self.degree = degree

    def __call__(self, x, y):
        return np.dot(x.T, y) ** self.degree

    def param_name(self):
        return "degree"


class LinearKernel(PolynomialKernel):
    def __init__(self, _=None):
        super().__init__(1)
        self.name = "Linear kernel"

    def param_name(self):
        return None


class GaussianKernel(Kernel):
    def __init__(self, gamma=0.1):
        super().__init__("Gaussian kernel")
        self.gamma = gamma

    def __call__(self, x, y):
        return np.exp(-self.gamma * np.linalg.norm((x - y) ** 2))

    def param_name(self):
        return "gamma"
