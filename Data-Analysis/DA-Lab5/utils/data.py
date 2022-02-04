from typing import Any

import yaml
import pandas as pd
import numpy as np
import ast


def reload_config(path: str) -> dict[str, Any]:
    with open(path) as stream:
        config_cur = yaml.safe_load(stream)
    return config_cur


def get_user_data_X_y():
    config = reload_config("conf.yaml")
    year = config['year']
    data = config['data']['user']

    df = pd.read_csv(data)
    df = df.loc[(df['gender'].notnull())]

    df = df.loc[(df['birthday'].notnull())]
    df.loc[:, 'age'] = pd.to_datetime(df['birthday'], format='%Y-%m-%d').apply(lambda time: year - time.year)
    df = df.drop(
        ['name', 'planning', 'watched', 'joined', 'last_online', 'location', 'favorites', 'total_entries', 'completed',
         'rewatched', 'genre_count', 'watching', 'plan_to_watch', 'days_watched', 'episodes_watched', 'birthday'],
        axis=1)
    df = df.loc[
        (df['mean_score'] > 5) & (df['mean_score'] < 9.5) & (df['dropped'] != 0) & (df['age'] > 10) & (df['age'] < 40)]

    def get_gender(gender: str) -> int:
        if gender == 'Male':
            return 1
        if gender == 'Female':
            return 2
        return 0

    df['gender'] = df['gender'].apply(get_gender)
    X_parameters = ["age", "mean_score", "on_hold", "dropped"]
    y_parameter = "gender"
    return df[X_parameters], df[y_parameter]


def get_nba_X_y():
    config = reload_config("conf.yaml")
    data = config['data']['nba']
    df = pd.read_csv(data)
    df = df.drop(['full_name', 'team', 'college', 'jersey'], axis=1)
    df.salary = df.salary.str.slice(1).apply(int)

    def get_weight(series: pd.Series) -> pd.Series:
        return series.str.extract(r'(\d+) lbs\. / (\d+(\.\d+|)) kg\.')[1].apply(
            float)

    def get_height(series: pd.Series) -> pd.Series:
        return series.str.extract(r'((\d+)-(\d+)) / (\d+(\.\d+|))')[3].apply(float)

    df = df.assign(weight_kg=df.weight.pipe(get_weight)).assign(height_m=df.height.pipe(get_height))
    # normalize for TSNE
    df.rating /= 100
    df.salary /= 4e8
    df.weight_kg /= 100
    df.height_m /= 2
    X_parameters = ["rating", "weight_kg", "height_m", "salary"]
    y_parameter = "draft_year"
    return df[X_parameters], df[y_parameter]


def get_vk_X_y():
    config = reload_config("conf.yaml")
    data = config['data']['vk']
    year = config['year']
    df = pd.read_csv(data)

    def date_to_age(bdate: str) -> int:
        date = bdate.split(".")
        if len(date) < 3:
            return None
        else:
            return year - int(date[2])

    def count_info(row: pd.Series) -> int:
        columns = ["activities", "interests", "music", "movies", "tv", "books", "about", "inspired_by", "religion"]
        return sum([1 if pd.isna(row[column]) else 0 for column in columns])

    df.posts = df.posts.apply(lambda ps: np.array(ast.literal_eval(ps)))

    df = df.assign(age=df.bdate.apply(date_to_age),
                   posts_count=df.posts.apply(len),
                   posts_len=df.posts.apply(lambda ps: int(np.median([len(p) for p in ps]) if len(ps) > 0 else 0)),
                   info_count=df.apply(count_info, axis=1))
    df = df[["age", "relation", "posts_count", "posts_len", "info_count", "sex"]]
    df = df[df.age.notnull()]
    df['sex'] = (df['sex'] == 'male').astype(int)
    df = df[df['relation'] != 0]
    df = df[df['posts_count'] > 0]
    X = df[['age', 'posts_count', 'info_count', 'sex']]
    y = df['relation'] - 1
    return X, y
