from utils.f_score import calculate_F_score
from utils.nadaray_watson import nadaray_watson


def regression(
        data,
        targets,
        distance_function,
        kernel_function,
        window_type,
        window,
):
    confusion_matrix = [[0 for _ in range(len(targets))] for _ in range(len(targets))]

    for i in range(len(data)):
        data_train = data.copy()
        data_test = data_train.pop(i)
        predictions = []

        for j in range(len(targets)):
            target_train = targets[j].copy()
            target_train.pop(i)

            if window_type == "variable":
                xs = sorted(zip(data_train, target_train),
                            key=lambda xy: distance_function(data_test, xy[0]))
                h = distance_function(targets[j], xs[window][0])
            else:
                h = window

            prediction = nadaray_watson(
                data_train,
                target_train,
                h,
                data_test,
                kernel_function,
                distance_function
            )
            predictions.append(prediction)

        reals = []
        for k in targets:
            reals.append(k[i])
        real = reals.index(max(reals))
        prediction = predictions.index(max(predictions))
        confusion_matrix[prediction][real] += 1

    return calculate_F_score(confusion_matrix)
