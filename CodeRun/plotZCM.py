#!/usr/bin/env python3
import matplotlib.pyplot as plt
import pprint
import pandas
ZCM_data = pandas.read_csv(  'ZCM_result.dat', delim_whitespace = True )
pprint.pprint(ZCM_data)
plt.figure()
plt.plot(ZCM_data['x'],ZCM_data['REAL(y)'])
# ZCM_data.plot()
plt.show()


