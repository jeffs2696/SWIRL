#!/usr/bin/env python
# coding: utf-8

# In[1]:

import re
import tikzplotlib
import glob
import os
import pandas as pd
import matplotlib as mpl
import matplotlib.ticker as mticker
import plotReportLib
from matplotlib import pyplot as plt
from plotReportLib import myfunctions as fcn
from tabulate import tabulate
from texttable import Texttable
import latextable

def extract_number(f):
    s = re.findall("\d+$",f)
    return (int(s[0]) if s else -1,f)

list_of_files = glob.glob('../01-mean-flow/mean-flow????.dat')
list_of_files2 = glob.glob('../04-EVanalysis/gammas????.dat')

last_file = (max(list_of_files,key=extract_number))
last_file2 = (max(list_of_files2,key=extract_number))
#getting mean flow file with highest grid number 
#getting number of grid points
#print(last_file)
#getting mean flow file with highest grid number 
#last_file2 = os.path.basename(list_of_files2[-1])
#getting number of grid points
#print(last_file2)

#max_grid = int(re.findall('\d+$',last_file)[0])


#max_grid = (max(list_of_files,key=extract_number))
#max_grid = (max(list_of_files2,key=extract_number))
#max_grid =int(re.findall('\d+$',last_file)[0]) 


#print(round(int(max_grid)/10))
#print(max_grid)
# importing data
flow_data    = fcn.importPlotData(last_file)
cv_wave_data = fcn.importPlotData(last_file2)
LEE_L2_data  = fcn.importPlotData('../02-method-of-manufactured-solutions/L2-LEE.dat')
LEE_ROC_data = fcn.importPlotData('../02-method-of-manufactured-solutions/ROC-LEE.dat')
SND_L2_data  = fcn.importPlotData('../02-method-of-manufactured-solutions/L2-sound_speed-.dat')
SND_ROC_data = fcn.importPlotData('../02-method-of-manufactured-solutions/ROC-sound_speed.dat')
#cv_wave_data = fcn.importPlotData('../04-EVanalysis/cv.waves.dat')
Delta_r    = LEE_ROC_data.Delta_r
LEE_ROC    = LEE_ROC_data.ROC
SND_ROC    = SND_ROC_data.ROC
LEE_L2    = LEE_L2_data.L2
SND_L2    = SND_L2_data.L2
GridPoints = LEE_L2_data.GridPoints

#print(cv_wave_data.idxmax(axis=0))
#
#table = Texttable()
#table.set_cols_align(["c"] * int())
#
#table.set_deco(Texttable.HEADER | Texttable.VLINES)
#rows =cv_wave_data.loc[:] 
#table.add_rows(rows)
#print('Tabulate Table:')
#print(tabulate(rows, headers='firstrow'))
#print('\nTexttable Table:')
#print(table.draw())
#
with open('../03-plotReport/tex-outputs/cv_wave_table.tex','w') as tex_file:
    contents = cv_wave_data.to_latex(index=False)
    tex_file.write(contents)

#print(tabulate(cv_wave_data.iloc[0:],headers =cv_wave_data.columns,tablefmt='latex'))



# In[3]:
markers = ['->', '-+', '-.', '-o', '--', 'v', 'x', 'X', 'D', '|']


plt.style.use('plot_style.txt')


# plot data

plt.plot(
        flow_data['radius'],
        flow_data['M_x'],
        label = '$M_{x}$',
        )

plt.plot(
        flow_data['radius'],
        flow_data['M_theta'],
        label = '$M_{\\theta}$' ,
        )

plt.plot(
        flow_data['radius'],
        (flow_data['M_x']**2+ flow_data['M_theta']**2)**0.5,
        label = '$M_{Total}$' ,
        )

# add details
plt.title('Flow Velocity')
plt.ylabel('Mach Number, M')
plt.xlabel(r'$\bar{r}$')
plt.legend()
plt.tight_layout()

#plt.show()
plt.grid(True)
tikzplotlib.save("tex-outputs/MachDistribution.tex",extra_axis_parameters= ['width=10cm'])

# In[4]:

def plot_measurement(args, kwargs,x_label,y_label):
    # Keyword arguments can be accessed as a normal dictionary
    mpl.rcParams['lines.linewidth'] = 2
    mpl.rcParams['lines.color'] = 'r'
    
    mpl.pyplot.plot(*args, **kwargs)
    mpl.pyplot.xlabel(x_label)
    mpl.pyplot.ylabel(y_label)
    mpl.pyplot.legend()


fig, ax = plt.subplots(1,1,figsize=(5,5)) 
plt.plot(
        flow_data['radius'],
        flow_data['A_expected'],
        markers[1%10], 
        markevery = 25,
        label ='Expected' ,
        linestyle = 'dashed')

plt.plot(         flow_data['radius'],flow_data['A_actual'],          label ='Actual',         linestyle = 'dotted')

plt.ylabel(r'$\bar{A}$')
plt.xlabel('Radius')
plt.grid(True)

plt.legend()
fig = mpl.pyplot.gcf()
fig.set_size_inches(18.5, 10.5, forward=True)
tikzplotlib.save("tex-outputs/SoundSpeedFromIntegration.tex",extra_axis_parameters=['width = 10cm','height=10cm'])


# In[6]:


fig, (ax1,ax2,ax3,ax4) = plt.subplots(4,1,sharex=True)

ax1.set_title('Perturbation Variables')
ax1.plot(         flow_data['radius'],flow_data['vR']          )
ax1.set_ylabel(r'$\bar{v}_r$')
ax2.plot(         flow_data['radius'],flow_data['vTh']          )
ax2.set_ylabel(r'$\bar{v}_{\theta}$')
ax3.plot(         flow_data['radius'],flow_data['vX']        )
ax3.set_ylabel(r'$\bar{v}_x$')
ax4.plot(         flow_data['radius'],flow_data['Pr']        )
ax4.set_ylabel(r'$\bar{p}$')
ax4.set_xlabel(r'$\bar{r}$')
tikzplotlib.save("tex-outputs/PerturbationVariables.tex",extra_axis_parameters= ['width=10cm','height=5cm'])



fig, (ax1,ax2) = plt.subplots(
        nrows=2,
        ncols=1,
        sharex=True)

ax1.semilogy(
        GridPoints[0:],
        SND_L2,
        markers[2%10],
        label='Speed Of Sound')

ax1.legend()
ax1.set_title('L2 Norm of the Error for the Manufactured Solution as a function of Grid Points')

ax2.semilogy(
        GridPoints[0:],
        LEE_L2,
        markers[2%10],
        label='LEE')

ax2.legend()

ax2.set_xlabel('Number of Gridpoints')

ax1.yaxis.set_major_formatter(mticker.ScalarFormatter())
ax2.yaxis.set_major_formatter(mticker.ScalarFormatter())

ax1.yaxis.set_minor_formatter(mticker.ScalarFormatter())
ax2.yaxis.set_minor_formatter(mticker.ScalarFormatter())

tikzplotlib.save("tex-outputs/L2.tex",figure="gcf",extra_axis_parameters= ['height=10cm'])
#,extra_axis_parameters=['yticklabel style={ /pgf/number format/fixed, #/pgf/number format/precision=5}','scaled y ticks=false']) 

# In[10]:


fig, (ax1,ax2) = plt.subplots(                       nrows=2,                       ncols=1,                              sharex=True)
ax1.semilogy(GridPoints[1:],SND_ROC,label='Speed Of Sound')
ax1.legend()
ax1.set_title('Rate Of Convergence')
ax2.semilogy(GridPoints[1:],LEE_ROC,label='LEE')
ax2.legend()
ax2.set_xlabel('Number of Gridpoints')
ax1.yaxis.set_major_formatter(mticker.ScalarFormatter())
ax2.yaxis.set_major_formatter(mticker.ScalarFormatter())

ax1.yaxis.set_minor_formatter(mticker.ScalarFormatter())
ax2.yaxis.set_minor_formatter(mticker.ScalarFormatter())

tikzplotlib.save("tex-outputs/ROC.tex",figure="gcf",extra_axis_parameters= ['height=10cm'])
#,extra_axis_parameters=['yticklabel style={ /pgf/number format/fixed, #/pgf/number format/precision=5}','scaled y ticks=false']) 

# In[ ]:





# In[8]:


#plt.semilogy(Delta_r,LEE_ROC)
fig, ax = plt.subplots(nrows =4, ncols=1,sharex=True,figsize=(10,4))

# can i loop though axes?
ax[0].set_ylabel('Radial')
ax[0].plot(flow_data['radius'],flow_data['S_1_e'],label='expected')
ax[0].plot(flow_data['radius'],flow_data['S_1_a'],label='actual', linestyle = 'dashed')
ax[0].legend(loc="upper right")
ax[1].set_ylabel('Tangential')
ax[1].plot(flow_data['radius'],flow_data['S_2_e'])
ax[1].plot(flow_data['radius'],flow_data['S_2_a'], linestyle = 'dashed')
ax[2].set_ylabel('Axial')
ax[2].plot(flow_data['radius'],flow_data['S_3_e'])
ax[2].plot(flow_data['radius'],flow_data['S_3_a'], linestyle = 'dashed')
ax[3].set_ylabel('Energy')
ax[3].plot(flow_data['radius'],flow_data['S_4_e'])
ax[3].plot(flow_data['radius'],flow_data['S_4_a'], linestyle = 'dashed')
tikzplotlib.save("tex-outputs/SourceTermData.tex",extra_axis_parameters=['width=10cm','height=5cm'])


# In[ ]:

fig, ax = plt.subplots(
        nrows=1,
        ncols=1,
        sharex=True,
        figsize=(10,4)
        )
plt.scatter(
        cv_wave_data['Re{gam/ak}'],
        cv_wave_data['Im{gam/ak}'],
        marker = 'o',
        s = 1)
tikzplotlib.save("tex-outputs/cv_wave.tex",extra_axis_parameters=['width=10cm','height=5cm'])
