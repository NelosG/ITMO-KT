import itertools

import numpy as np
from tqdm import tqdm
from utils.regression import regression


class LeaveOneOut:
    def __init__(self, distance_functions, kernel_functions):
        self.targets = None
        self.data = None
        self.best_distance_function = None
        self.best_kernel_function = None
        self.best_window_type = None
        self.best_window = None
        self.distance_functions = distance_functions
        self.kernel_functions = kernel_functions
        self.window_types = ["variable", "fixed"]

    def fit(self, data, targets):
        self.data = data
        self.targets = targets

    def predict(self):
        max_F_score = 0

        sqrt_D = np.sqrt(len(self.data))

        for distance_function, kernel_function, window_type in tqdm(
                itertools.product(self.distance_functions,
                                  self.kernel_functions,
                                  self.window_types),
                total=len(self.distance_functions) * len(self.kernel_functions) * len(self.window_types)):
            if window_type == "variable":
                windows = [i for i in range(1, int(np.ceil(sqrt_D)))]  # [1, sqrt_D]
            else:
                R_D = max(
                    distance_function(x, y)
                    for x, y in itertools.product(self.data, self.data))
                R_D_Div = R_D / sqrt_D
                windows = [R_D_Div * i for i in
                           range(1, int(np.ceil(sqrt_D)))]  # [ R_D_Div, R_D ] с шагом R_D_Div
            for window in windows:
                F_score = regression(self.data, self.targets, distance_function,
                                     kernel_function, window_type, window)
                if F_score >= max_F_score:
                    max_F_score = F_score
                    self.best_distance_function = distance_function
                    self.best_kernel_function = kernel_function
                    self.best_window_type = window_type
                    self.best_window = window
        print("----------")
        print("Distance: " + self.best_distance_function.__name__)
        print("Kernel: " + self.best_kernel_function.__name__)
        print("Window Type: " + self.best_window_type)
        if self.best_window_type == "variable":
            print("Neibhours: " + str(self.best_window))
        else:
            print("Window Width: " + str(self.best_window))
        print("max F score: " + str(max_F_score))
        return max_F_score
