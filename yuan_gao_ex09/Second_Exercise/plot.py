import matplotlib.pyplot as plt
from math import *

biggest = 0.0
x=[]
y=[]
for k in xrange(2,17,1):
	x = []
	with open("result_%d.out"%k,'r') as f:
		line= f.readline()
		while line:
			if(line[0] != 'g' and line[0] != '\n'):
				x.append(float(line.split(' :')[1]))
			line= f.readline() 
	y.append(max(x))
print y
plt.plot(range(2,17,1),y)
plt.xlabel('The numberof tested nodes')
plt.ylabel('The longest time of all nodes')

plt.show()
