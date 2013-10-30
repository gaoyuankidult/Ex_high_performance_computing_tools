import matplotlib.pyplot as plt
from math import *

x=[]
y=[]
with open("data.dat",'r') as f:
    line= f.readline()
    while line:
        x.append(int(line))
        y.append(int(f.readline()))
        line= f.readline() 
plt.bar(x,y)
plt.xlabel('random numbers')
plt.ylabel('occurances')

plt.show()
