import math
inp = open ('input.txt', 'r')
out = open ('output.txt', 'w')
num = []
num = inp.readline().split()
vx = float(num[0])
vy = float(num[1])
vz = 0
num = inp.readline().split()
ax = float(num[0])
ay = float(num[1])
az = 0
num = inp.readline().split()
mx = float(num[0])
my = float(num[1])
mz = 1
num = inp.readline().split()
wx = float(num[0])
wy = float(num[1])
wz = 0
inp.close()

nop = 0
p = 666
m = 777

def l(x, a, b, c, d):
    return float((x*d-a*d+b*c)/c)
def alf(a, b, c, x, y, z):
    cos = float((a*x+b*y+c*z)/(math.sqrt(a**2+b**2+c**2)*math.sqrt(x**2+y**2+z**2)))
    return math.degrees(math.acos(cos))

if ((wx - vx) * ay + (wy - vy) * (- ax)) > 0:
    nop = -1
else:
    if ((wx - vx) * ay + (wy - vy) * (- ax)) < 0:
        nop = 1
if nop == 1:
    p = alf(-ay, ax, az, float(wx - vx), float(wy - vy), float(wz - vz))
    if ((wx - vx) * ax + (wy - vy) * ay) < 0:
        p = -p
    if math.fabs(p) > 60:
            nop = 0
    if mx == my == 0 or alf(mx, my, mz, -ay, ax, az)== 0:
        m = 0
    else:
        m = -(90 - round(alf(mx, my, mz, -ay, ax, az)))
    if math.fabs(m) > 60:
        nop = 0
else:
    if nop == -1:
        p = alf(ay, -ax, az, float(wx - vx), float(wy - vy), float(wz - vz))
        if ((wx - vx) * ax + (wy - vy) * ay) < 0:
            p = -p
        if math.fabs(p) > 60:
            nop = 0
        if mx == my == 0 or alf(mx, my, mz, -ay, ax, az)== 0:
            m = 0
        else:
            m = -(90 - round(alf(mx, my, mz, ay, -ax, az)))
        if math.fabs(m) > 60:
            nop = 0
out.writelines(str(nop) + '\n')
print(str(nop))
if nop == 1 or nop == -1:
    out.writelines(str(p) + '\n')
    out.writelines(str(m) + '\n')
    out.writelines("welcome to Hell")
    print(str(p))
    print(str(m))
    print("...")

out.close()
