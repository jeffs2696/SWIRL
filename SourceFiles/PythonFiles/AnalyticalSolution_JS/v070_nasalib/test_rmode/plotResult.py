#!/usr/bin/env python3
import matplotlib
import matplotlib.pyplot as plt
import pandas as pd
import pprint 
import scipy as scip


filename = 'radial_mode_data.dat'
data = \
        pd.read_csv(filename, delim_whitespace = True)
pprint.pprint(data)

fig, ax = plt.subplots()
plt.plot(data['radius'],data['pressure'],label = 'FORTRAN')
plt.xlabel('Radius [-]?')
plt.ylabel('Pressure')
plt.show()

# AMN = 0.5
# BMN = 0.5

# pressure_python = AMN*scip.special.jv(2,data['radius']) + BMN*scip.special.yv(2,data['radius'])
# plt.plot(data['radius'],pressure_python,'o',label = 'Python Output')
# plt.legend()
# plt.show()

