import matplotlib.pyplot as plt
from math import *

runtime_singel=[]
runtime_double=[]
dimension = []
with open("data_signle_&_double.dat",'r') as f:
    line= f.readline()
    while line:
        dimension.append(int(line))
        runtime_singel.append(float(f.readline()))
        runtime_double.append(float(f.readline()))

        line= f.readline()

plt.plot(dimension,runtime_singel,label = "single precision")
plt.plot(dimension,runtime_double,'r',label="double precision")
plt.xlabel('number os loops')
plt.ylabel('time consumption(s)')
plt.legend( loc='upper left', numpoints = 1 )
#plt.loglog(runtime_do,runtime_matmul,'ro',basex=2,basey=2)
#plt.xlabel('time consumption of do loop multiplication')
#plt.ylabel('time consumption of matmul matiplication')

plt.show()
