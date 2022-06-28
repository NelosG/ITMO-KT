def nadaray_watson(x, y, h, u, kernel_function, distance_function):
    if h != 0:
        numerator = 0
        denominator = 0
        for i in range(len(x)):
            kernel_res = kernel_function(distance_function(x[i], u) / h)
            denominator += kernel_res
            numerator += y[i] * kernel_res
        if denominator != 0:
            return numerator / denominator
        else:
            summ = 0
            for i in range(len(x)):
                summ += y[i]
            return summ / len(x)
    else:
        objects = []
        if x[0] == u:
            for i in range(len(x)):
                if x[i] == u:
                    objects.append(i)
        else:
            objects = list(range(0, len(x)))
        summ = 0
        for i in objects:
            summ += y[i]
        return summ / len(objects)
