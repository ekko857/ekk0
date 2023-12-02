import numpy as np
a = np.array(range(5))
a
print(type(a))
print(type(a[0]))

a = np.array(range(5), float)
a
a.shape

x = np.arange(5)
x.shape
x.dtype

b = np.array([i for i in range(10) if i % 2 == 1]
             )
c = b.tolist()
c

mat = np.array([[0,1], [2,3]])
mat.dtype
type(mat)

mat[1]
mat[:,1]
mat[0,0]


np.ones((4,2))

np.zeros((4,4))

m = np.identity((4))
m

m.fill(16)
m

mm = np.arange(16)
mm
mm = mm.reshape(4,4)
mm.transpose()
mm + mm.transpose()
mm - mm.transpose()

mm * mm.transpose() # Note that this is element-wise multiplication
mm // mm.transpose()
mm.dot(mm) # No this is matric multiplication, or the dot product


import scipy as sc
sc.stats.norm.rvs(size = 10)

np.random.seed(1224)
sc.stats.norm.rvs(size = 10)

sc.stats.norm.rvs(size = 5, random_state = 1234)
sc.stats.randint.rvs(0, 10, size = 7)

sc.stats.randint.rvs(0, 10, size = 7, random_state=1234)
sc.stats.randint.rvs(0, 10, size = 7, random_state=3445) # a different seed

import scipy.integrate as integrate
y = np.array([5, 20, 18, 19, 18, 7, 4]) # The y values; can also use a python list here

import matplotlib.pylab as p
p.plot(y)
p.show()

area = integrate.trapz(y, dx = 1)
print("area =", area)
area = integrate.trapz(y, dx = 2)
print("area =", area)
area = integrate.trapz(y, dx = 3)
print("area =", area)
area = integrate.simps(y, dx = 1)
print("area =", area)


def dCR_dt(pops, t = 0):

    R = pops[0]
    C = pops[1]
    dRdt = r * R - a * R * C
    dCdt = -z * C + e * a * R * C 
    return np.array([dRdt, dCdt])

type(dCR_dt)

r = 1.
a = 0.1
z = 1.5
e = 0.75

t = np.linspace(0, 15, 1000)
R0 = 10
C0 = 5
RC0 = np.array([R0, C0])


pops, infodict = integrate.odeint(dCR_dt, RC0, t, full_output=True)
pops
type(infodict)
infodict.keys()
infodict['message']
import matplotlib.pylab as p
f1 = p.figure()
p.plot(t, pops[:,0], 'g-', label='Resource density') # Plot
p.plot(t, pops[:,1]  , 'b-', label='Consumer density')
p.grid()
p.legend(loc='best')
p.xlabel('Time')
p.ylabel('Population density')
p.title('Consumer-Resource population dynamics')
p.show()# To display the figure