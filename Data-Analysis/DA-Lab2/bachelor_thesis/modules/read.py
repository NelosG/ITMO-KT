import numpy as np


def read_x_and_y(path: str):
    with open(path) as f:
        lines = f.readlines()
    x = []
    y = []
    for line in lines:
        if line[0] == 'h':
            y.append(0)
            string = line[4: len(line) - 4]
            ss = ''
            for s in string:
                if s == ' ' or (ord('a') <= ord(s) <= ord('z')) or (ord('A') <= ord(s) <= ord('Z')):
                    ss += s
            x.append(ss)
        else:
            y.append(1)
            string = line[5: len(line) - 4]
            ss = ''
            for s in string:
                if s == ' ' or (ord('a') <= ord(s) <= ord('z')) or (ord('A') <= ord(s) <= ord('Z')):
                    ss += s
            x.append(ss)
    y = np.array(y)
    return x, y
