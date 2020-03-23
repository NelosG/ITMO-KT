import math
 
 
def parallelism(V, Plane, i):
    return (V[0] * Plane[i][0] + V[1] * Plane[i][1] + V[2] * Plane[i][2]) != 0
 
 
def VecAngle(A, B):
    return ((A[0] * B[0] + A[1] * B[1] + A[2] * B[2]) / (math.sqrt(A[0] * A[0] + A[1] * A[1] + A[2] * A[2]) * math.sqrt(
        B[0] * B[0] + B[1] * B[1] + B[2] * B[2])))
 
 
def absVec(A):
    return math.sqrt(A[0] * A[0] + A[1] * A[1] + A[2] * A[2])
 
 
def getVector(A, B):
    Vector = []
    Vector.append(A[0] - B[0])
    Vector.append(A[1] - B[1])
    Vector.append(A[2] - B[2])
    return Vector
 
 
def getReverse(V, Normal):
    Ans = []
    if (VecAngle(V, Normal) <= 0):
        for i in range(3):
            Normal[i] *= (-1)
    pr = (V[0] * Normal[0] + V[1] * Normal[1] + V[2] * Normal[2]) / absVec(Normal)
    Pr = []
    k = absVec(Normal) / pr
    Pr.append(Normal[0] / k)
    Pr.append(Normal[1] / k)
    Pr.append(Normal[2] / k)
    Sub = []
    Sub.append(2 * (V[0] - Pr[0]))
    Sub.append(2 * (V[1] - Pr[1]))
    Sub.append(2 * (V[2] - Pr[2]))
    Ans.append(Sub[0] - V[0])
    Ans.append(Sub[1] - V[1])
    Ans.append(Sub[2] - V[2])
    return Ans
 
 
def getPointCube(B, D, C):
    V1 = getVector(D, C)
    Ans = []
    Ans.append(V1[0] + B[0])
    Ans.append(V1[1] + B[1])
    Ans.append(V1[2] + B[2])
    return Ans
 
 
def getPoints(Plane, V, PointIn, n):
    Points = []
    for i in range(n):
        if (parallelism(V, Plane, i)):
            Points.append([])
            Points[-1].append(
                ((-1) * (Plane[i][0] * PointIn[0] + Plane[i][1] * PointIn[1] + Plane[i][2] * PointIn[2] + Plane[i][3]) /
                 (V[0] * Plane[i][0] + V[1] * Plane[i][1] + V[2] * Plane[i][2])) * V[0] + PointIn[0])
            Points[-1].append(
                ((-1) * (Plane[i][0] * PointIn[0] + Plane[i][1] * PointIn[1] + Plane[i][2] * PointIn[2] + Plane[i][3]) /
                 (V[0] * Plane[i][0] + V[1] * Plane[i][1] + V[2] * Plane[i][2])) * V[1] + PointIn[1])
            Points[-1].append(
                ((-1) * (Plane[i][0] * PointIn[0] + Plane[i][1] * PointIn[1] + Plane[i][2] * PointIn[2] + Plane[i][3]) /
                 (V[0] * Plane[i][0] + V[1] * Plane[i][1] + V[2] * Plane[i][2])) * V[2] + PointIn[2])
            if(PointIn == Points[-1]):
                Points.pop()
    return Points
 
 
def getPlane(A, B, C):
    Plane = []
    Plane.append((A[1] - B[1]) * (B[2] - C[2]) - (B[1] - C[1]) * (A[2] - B[2]))
    Plane.append((-1) * ((A[0] - B[0]) * (B[2] - C[2]) - (B[0] - C[0]) * (A[2] - B[2])))
    Plane.append((A[0] - B[0]) * (B[1] - C[1]) - (B[0] - C[0]) * (A[1] - B[1]))
    Plane.append((-1) * A[0] * ((A[1] - B[1]) * (B[2] - C[2]) - (B[1] - C[1]) * (A[2] - B[2])) + A[1] * (
                (A[0] - B[0]) * (B[2] - C[2])
                - (B[0] - C[0]) * (A[2] - B[2])) - A[2] * (
                             (A[0] - B[0]) * (B[1] - C[1]) - (B[0] - C[0]) * (A[1] - B[1])))
    return Plane
 
 
fin = open("input.txt", "r")
ff = open("output.txt", "w")
A = list(map(float, fin.readline().split()))
B = list(map(float, fin.readline().split()))
C = list(map(float, fin.readline().split()))
D = list(map(float, fin.readline().split()))
A1 = getPointCube(A, D, C)
B1 = getPointCube(A1, C, B)
V = list(map(float, fin.readline().split()))
PointIn = list(map(float, fin.readline().split()))
E = int(fin.readline())
n = int(fin.readline())
Mirror = []
x = 3 * n
for i in range(x):
    Mirror.append([])
    T = list(map(float, fin.readline().split()))
    for j in range(3):
        Mirror[i].append(T[j])
PlaneMirror = []
for i in range(n):
    PlaneMirror.append([])
    PlaneMirror[i] = getPlane(Mirror[3 * i], Mirror[3 * i + 1], Mirror[3 * i + 2])
PlaneCube = []
for i in range(6):
    PlaneCube.append([])
PlaneCube[0] = getPlane(A, B, C)
PlaneCube[1] = getPlane(A, A1, B1)
PlaneCube[2] = getPlane(A, A1, B)
PlaneCube[3] = getPlane(B, C, D)
PlaneCube[4] = getPlane(C, D, B1)
PlaneCube[5] = getPlane(A1, B1, D)
Junction = []
min = 1000000000000
cur = 0
while (E > 0):
    PointsWithMirror = getPoints(PlaneMirror, V, PointIn, n)
    PointsWithCube = getPoints(PlaneCube, V, PointIn, 6)
    for i in range(len(PointsWithMirror)):
        if VecAngle(V, getVector(PointsWithMirror[i], PointIn)) > 0 and absVec(
                getVector(PointsWithMirror[i], PointIn)) < min:
            min = absVec(getVector(PointsWithMirror[i], PointIn))
            Junction = PointsWithMirror[i]
    for i in range(len(PointsWithCube)):
        # print(*PointsWithMirror[i])
        if (VecAngle(V, getVector(PointsWithCube[i], PointIn)) > 0 and absVec(
                getVector(PointsWithCube[i], PointIn)) < min):
            cur += 1
            min = absVec(getVector(PointsWithCube[i], PointIn))
            Junction = PointsWithCube[i]
    if (cur > 0):
        ff.write(str(1) + "\n")
        ff.write(str(E) + "\n")
        ff.write(str(Junction[0]) + " " + str(Junction[1]) + " " + str(Junction[2]) + "\n")
        AI = getVector(Junction, PointIn)
        ff.write(str(AI[0]) + " " + str(AI[1]) + " " + str(AI[2]) + "\n")
        exit(0)
    Normal = []
    min = 1000000000000
    for i in range(len(PlaneMirror)):
        if (PlaneMirror[i][0] * Junction[0] + PlaneMirror[i][1] * Junction[1] + PlaneMirror[i][2] * Junction[2] +
            PlaneMirror[i][3]) == 0:
            Normal.append(PlaneMirror[i][0])
            Normal.append(PlaneMirror[i][1])
            Normal.append(PlaneMirror[i][2])
    if (VecAngle(Normal, V) == -1):
        V = Normal
    if (VecAngle(Normal, V) == 1):
        V = [-1*Normal[i] for i in range(3)]
    else :
        V = getReverse(V, Normal)
    PointIn = Junction
    E -= 1
ff.write(str(0) + "\n")
for i in range(3):
    ff.write(str(PointIn[i]) + " ")
fin.close()
ff.close()