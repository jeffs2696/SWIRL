
#!/usr/bin/env python3
import matplotlib
import matplotlib.pyplot as plt
import pandas as pd
import pprint 
import scipy as scip
import numpy as np

debug_flag = True#False

filename = 'S_sorted_egv.dat'
filename1 = 'S_sorted_first_egv.dat'
data = \
        pd.read_csv(filename, delim_whitespace = True)

data1 = \
        pd.read_csv(filename1, delim_whitespace = True)

if debug_flag:
   pprint.pprint(data)


fig, axs = plt.subplots(2,2)
np = int(len(data['i'])/4)

axs[0,0].plot(data['i'].iloc[1:np],data['S_real'].iloc[1:np])
axs[0,1].plot(data['i'].iloc[np+1:2*np],data['S_real'].iloc[np+1:np*2])


axs[1,0].plot(data['i'].iloc[2*np+1:3*np],data['S_real'].iloc[2*np+1:3*np])

axs[1,1].plot(data['i'].iloc[3*np+1:4*np],data['S_real'].iloc[3*np+1:4*np])

plt.suptitle('First Eigenvalue/Vector Residual')
fig, axs = plt.subplots(2,2)
np = int(len(data['i'])/4)

plt.suptitle('First cut on mode Eigenvalue/Vector Residual')
axs[0,0].plot(data1['i'].iloc[1:np],data1['S_imag'].iloc[1:np])

axs[0,1].plot(data1['i'].iloc[np+1:2*np],data1['S_imag'].iloc[np+1:np*2])


axs[1,0].plot(data1['i'].iloc[2*np+1:3*np],data1['S_imag'].iloc[2*np+1:3*np])

axs[1,1].plot(data1['i'].iloc[3*np+1:4*np],data1['S_imag'].iloc[3*np+1:4*np])





plt.show()



