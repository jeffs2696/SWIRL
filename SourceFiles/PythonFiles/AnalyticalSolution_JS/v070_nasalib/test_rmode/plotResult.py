#!/usr/bin/env python3
import matplotlib
import matplotlib.pyplot as plt
import pandas as pd
import pprint 

filename = 'radial_mode_data.dat'
data = \
        pd.read_csv(filename, delim_whitespace = True)
pprint.pprint(data)


plt.plot(data['radius'],data['pressure'])
plt.xlabel('Radius [-]?')
plt.ylabel('Pressure')

plt.show()
