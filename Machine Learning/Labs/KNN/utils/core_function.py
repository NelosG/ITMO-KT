# CORE FUNCTIONS

def uniform_f(u):
    return 0.5 if abs(u) < 1 else 0


def triangular_f(u):
    return (1 - abs(u)) if abs(u) < 1 else 0


def epanechnikov_f(u):
    return 0.75 * (1 - u * u) if abs(u) < 1 else 0


def quartic_f(u):
    return 15 / 16 * (1 - u * u) ** 2 if abs(u) < 1 else 0
