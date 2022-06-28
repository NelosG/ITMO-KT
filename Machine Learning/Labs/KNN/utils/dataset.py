def normalize(dataset, excluded_name):
    for feature in dataset.loc[:, dataset.columns != excluded_name]:
        dataset[feature] = (dataset[feature] - dataset[feature].min()) / (
                dataset[feature].max() - dataset[feature].min())
    return dataset


def one_hot(dataset, name_of_column):
    count_of_classes = get_count(dataset, name_of_column)
    classes = [[0] * len(dataset) for _ in range(count_of_classes)]
    for i in range(len(dataset)):
        classes[dataset[name_of_column][i] - 1][i] = 1
    for i in range(count_of_classes):
        new_name = "class_" + str(i + 1)
        dataset[new_name] = classes[i]
    return dataset.drop(name_of_column, axis=1)


def split_target(dataset, count_of_classes):
    targets = []
    data = dataset
    for i in range(count_of_classes):
        name = "class_" + str(i + 1)
        data = data.drop([name], axis=1)
        targets.append(dataset[name].values.tolist())
    data = data.values.tolist()
    return data, targets


def get_count(dataset, name_of_column):
    return dataset[name_of_column].max()
