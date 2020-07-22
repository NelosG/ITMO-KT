def read_matrix(n, m, matrix_one):
    matrix_array = [[] for i in range(n)]
    for i in range(n * m):
        matrix_array[i // m].append(float(matrix_one[i]))
    return matrix_array


def matrix_sum(first_matrix, second_matrix):
    nFirst = len(first_matrix)
    mFirts = len(first_matrix[0])
    nSecond = len(second_matrix)
    mSecond = len(second_matrix[0])
    result_sum = []
    for i in range(nFirst):
        result_sum.append([0] * mFirts)
    if (nFirst == nSecond) and (mFirts == mSecond):
        for i in range(nFirst):
            for j in range(mFirts):
                result_sum[i][j] = first_matrix[i][j] + second_matrix[i][j]
    else:
        fout = open('output.txt', 'w')
        fout.write('1')
        exit()
    return result_sum


def matrix_multi_number(matrix, number):
    n = len(matrix)
    m = len(matrix[0])
    result_multi = []
    for i in range(n):
        result_multi.append([0] * m)
    for i in range(n):
        for j in range(m):
            result_multi[i][j] = number * matrix[i][j]
    return result_multi


def matrix_multi_matrix(first_matrix, second_matrix):
    nFirst = len(first_matrix)
    mFirts = len(first_matrix[0])
    nSecond = len(second_matrix)
    mSecond = len(second_matrix[0])
    result_multi = []
    for i in range(nFirst):
        result_multi.append([0] * mFirts)
    if mFirts == nSecond:
        for k in range(nFirst):
            for i in range(mFirts):
                for j in range(nSecond):
                    result_multi[k][i] += first_matrix[k][j] * second_matrix[j][i]
    else:
        fout = open('output.txt', 'w')
        fout.write('0')
        exit()
    return result_multi


def matrix_transpose(matrix):
    n = len(matrix)
    m = len(matrix[0])
    result_transpose = []
    for i in range(m):
        result_transpose.append([0] * n)
    for i in range(m):
        for j in range(n):
            result_transpose[i][j] = matrix[j][i]
    return result_transpose


fin = open('input.txt')
alpha, beta = map(float, fin.readline().split())  

nA, mA = map(int, fin.readline().split())
A = read_matrix(nA, mA, fin.readline().split())

nB, mB = map(int, fin.readline().split())
B = read_matrix(nB, mB, fin.readline().split())

nC, mC = map(int, fin.readline().split())
C = read_matrix(nC, mC, fin.readline().split())

nD, mD = map(int, fin.readline().split())
D = read_matrix(nD, mD, fin.readline().split())

nF, mF = map(int, fin.readline().split())
F = read_matrix(nF, mF, fin.readline().split())
X = matrix_sum(matrix_multi_matrix(matrix_multi_matrix(C, matrix_transpose(
    matrix_sum(matrix_multi_number(A, alpha), matrix_multi_number(matrix_transpose(B), beta)))), D),
               matrix_multi_number(F, -1))
fout = open('output.txt', 'w')
fout.write('1\n')
fout.write(str(len(X)) + ' ' + str(len(X[0])) + '\n')
for i in X:
    for j in i:
        fout.write(str(j) + " ")
    fout.write('\n')
