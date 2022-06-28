def calculate_F_score(confusion_matrix):
    k = len(confusion_matrix)
    true_positive = [0] * k
    false_positive = [0] * k
    false_negative = [0] * k
    count_classes = [[0] * k] * k
    recall = [0.0] * k
    precision = [0.0] * k
    F_score = [0.0] * k
    total_sum = 0
    F_score_sum = 0

    for i in range(k):
        for j in range(k):
            total_sum += confusion_matrix[i][j]
            count_classes[i] += confusion_matrix[i][j]

    for i in range(k):
        for j in range(k):
            if i == j:
                true_positive[i] = confusion_matrix[i][i]
            else:
                false_positive[i] += confusion_matrix[j][i]
                false_negative[i] += confusion_matrix[i][j]

        recall[i] = 0 if (true_positive[i] + false_negative[i]) == 0 else float(true_positive[i]) / (
                    true_positive[i] + false_negative[i])
        precision[i] = 0 if (true_positive[i] + false_positive[i]) == 0 else float(true_positive[i]) / (
                    true_positive[i] + false_positive[i])
        F_score[i] = 0 if (precision[i] + recall[i]) == 0 else 2 * (
                    (precision[i] * recall[i]) / (precision[i] + recall[i]))

        F_score_sum += F_score[i] * count_classes[i]

    F_score_average = F_score_sum / total_sum

    return F_score_average
