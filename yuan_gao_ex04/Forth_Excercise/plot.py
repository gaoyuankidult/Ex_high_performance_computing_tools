import matplotlib.pyplot as plt
from math import *

runtime_do=[]
runtime_matmul=[]
dimension = []
with open("data_completed.dat",'r') as f:
    line= f.readline()
    while line:
        dimension.append(int(line))
        runtime_matmul.append(float(f.readline()))
        runtime_do.append(float(f.readline()))

        line= f.readline()

#plt.plot(dimension,runtime_do,'r',label="do loop")
#plt.plot(dimension,runtime_matmul,label = "matmul")
#plt.xlabel('dimension')
#plt.ylabel('time consumption')
#plt.legend( loc='upper left', numpoints = 1 )
plt.loglog(runtime_do,runtime_matmul,'ro',basex=2,basey=2)
plt.xlabel('time consumption of do loop multiplication')
plt.ylabel('time consumption of matmul matiplication')

plt.show()
