#!/usr/bin/env python
# coding: utf-8

# In[1]:

import re
import tikzplotlib
import glob
import os
import pandas as pd
import matplotlib as mpl
mpl.use('pgf')
import matplotlib.ticker as mticker
import plotReportLib
from matplotlib import pyplot as plt
import matplotlib as mpl
from plotReportLib import myfunctions as fcn
from tabulate import tabulate
from texttable import Texttable
import latextable
from my_plot import set_size

plt.style.use('seaborn-whitegrid')
width =345

tex_fonts = {
        # Use LaTeX to write all text
        "text.usetex": True,
        "font.family": "serif",
        # Use 10pt font in plots, to match 10pt font in document
        "axes.labelsize": 10,
        "font.size": 10,
        # Make the legend/label fonts a little smaller
        "legend.fontsize": 8,
        "xtick.labelsize": 8,
        "ytick.labelsize": 8
        }
mpl.rcParams.update(tex_fonts) 


def extract_number(f):
    s = re.findall("\d+$",f)
    return (int(s[0]) if s else -1,f)
#pgf_with_pdflatex = {
#        "pgf.texsystem": "lualatex",
#        "pgf.preamble": [
#            r'\usepackage{amsmath}',
#            r'\usepackage[scientific-notation=true]{siunitx}',
#            ##      r"\usepackage[utf8x]{inputenc}",
#            ##      r"\usepackage[T1]{fontenc}",
#            ]
#        }
#mpl.rcParams.update(pgf_with_pdflatex)
#


















##
#mpl.rcParams['lines.linewidth'] = 4
#mpl.rcParams.update({
#    "pgf.texsystem": "xelatex",
#    'font.family': 'serif',
#    'text.usetex': True,
#    'pgf.rcfonts': False,
#    })
#
##
#pgf_with_pdflatex = {
#        "pgf.texsystem": "lualatex",
#        "pgf.preamble": [
#            r'\usepackage{amsmath}',
#            r'\usepackage[scientific-notation=true]{siunitx}',
#            ##      r"\usepackage[utf8x]{inputenc}",
#            ##      r"\usepackage[T1]{fontenc}",
#            ]
#        }
#mpl.rcParams.update(pgf_with_pdflatex)
list_of_files = glob.glob('../01-mean-flow/mean-flow????.dat')
#list_of_files2 = glob.glob('../04-EVanalysis/gammas????.dat')
#list_of_files3 = glob.glob('../04-EVanalysis/gam????.acc')

last_file = (max(list_of_files,key=extract_number))
#last_file2 = (max(list_of_files2,key=extract_number))
#last_file3 = (max(list_of_files3,key=extract_number))

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
#gammas_data = fcn.importPlotData(last_file2)
#gam_acc_data = fcn.importPlotData(last_file3)

#cv_wave_data = fcn.importPlotData('../04-EVanalysis/cv.waves.dat')
#gam_non_acc_data = fcn.importPlotData('../04-EVanalysis/gam.non.acc')

LEE_L2_data  = fcn.importPlotData('../02-method-of-manufactured-solutions/L2-LEE.dat')
LEE_ROC_data = fcn.importPlotData('../02-method-of-manufactured-solutions/ROC-LEE.dat')
SND_L2_data  = fcn.importPlotData('../02-method-of-manufactured-solutions/L2-sound_speed-.dat')
SND_ROC_data = fcn.importPlotData('../02-method-of-manufactured-solutions/ROC-sound_speed.dat')

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
#with open('../03-plotReport/tex-outputs/gammas.table.tex','w') as tex_file:
#    contents = gammas_data.to_latex(index=False)
#    tex_file.write(contents)
#
#with open('../03-plotReport/tex-outputs/cv.waves.table.tex','w') as tex_file:
#    contents = cv_wave_data.to_latex(index=True)
#    tex_file.write(contents)
#
#with open('../03-plotReport/tex-outputs/gam.acc.table.tex','w') as tex_file:
#    contents = gam_acc_data.to_latex(index=False)
#    tex_file.write(contents)
#
#with open('../03-plotReport/tex-outputs/gam.non.acc.table.tex','w') as tex_file:
#    contents = gam_non_acc_data.to_latex(index=False)
#    tex_file.write(contents)
#

#print(tabulate(cv_wave_data.iloc[0:],headers =cv_wave_data.columns,tablefmt='latex'))



# In[3]:
markers = ['->', '-+', '-.', '-o', '--', 'v', 'x', 'X', 'D', '|']




fig, ax = plt.subplots(1, 1, figsize=set_size(width))
# plot data

ax.plot(
        flow_data['radius'],
        flow_data['M_x'],
        label = '$M_{x}$',
        )

ax.plot(
        flow_data['radius'],
        flow_data['M_theta'],
        label = '$M_{\\theta}$' ,
        )

ax.plot(
        flow_data['radius'],
        (flow_data['M_x']**2+ flow_data['M_theta']**2)**0.5,
        label = '$M_{Total}$' ,
        )

# add details
ax.set_title('Flow Velocity')
ax.set_ylabel('Mach Number, M')
ax.set_xlabel(r'$\bar{r}$')
ax.legend()
#ax.tight_layout()

#plt.show()
ax.grid(True)
plt.savefig('tex-outputs/MachDistribution.pdf', 
        format = 'pdf', 
        bbox_inches='tight')

#tikzplotlib.save("tex-outputs/MachDistribution.tex",extra_axis_parameters= ['width=10cm'])

fig, ax = plt.subplots(1,1,figsize=set_size(width)) 
plt.plot(
        flow_data['radius'],
        flow_data['A_expected'],
        label ='Expected' ,
        linewidth = 4,
        linestyle = 'dashed')

plt.plot(
        flow_data['radius'],
        flow_data['A_actual'],
        label ='Actual',
        linestyle = 'dotted')

plt.ylabel(r'$\bar{A}$')
plt.xlabel('Radius')
plt.grid(True)

plt.legend()
#fig = mpl.pyplot.gcf()
#fig.set_size_inches(18.5, 10.5, forward=True)
#
plt.savefig('tex-outputs/SoundSpeedFromIntegration.pdf',
        format = 'pdf', 
        bbox_inches='tight')

#tikzplotlib.save("tex-outputs/SoundSpeedFromIntegration.tex",extra_axis_parameters=['width = 10cm','height=10cm'])


# In[6]:


fig, (ax1,ax2,ax3,ax4) = plt.subplots(4,1,sharex=True,figsize=set_size(width))

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
#
plt.savefig('tex-outputs/PerturbationVariables.pdf',
        format = 'pdf', 
        bbox_inches='tight')

#tikzplotlib.save("tex-outputs/PerturbationVariables.tex",extra_axis_parameters= ['width=10cm','height=5cm'])



fig, (ax1,ax2) = plt.subplots(
        nrows=2,
        ncols=1,
        figsize=set_size(width),
        sharex=True)

ax1.loglog(
        GridPoints[0:],
        SND_L2,
        markers[2%10],
        label='Speed Of Sound')

ax1.legend()

ax2.loglog(
        GridPoints[0:],
        LEE_L2,
        markers[2%10],
        label='LEE')

ax2.legend()

ax2.set_xlabel('Number of Gridpoints')

#ax1.yaxis.set_major_formatter(mticker.ScalarFormatter())
#ax2.yaxis.set_major_formatter(mticker.ScalarFormatter())
#
#ax1.yaxis.set_minor_formatter(mticker.ScalarFormatter())
#ax2.yaxis.set_minor_formatter(mticker.ScalarFormatter())
#
#
ax1.xaxis.set_major_formatter(mticker.ScalarFormatter())
ax2.xaxis.set_major_formatter(mticker.ScalarFormatter())
#
#ax1.xaxis.set_minor_formatter(mticker.ScalarFormatter())
#ax2.xaxis.set_minor_formatter(mticker.ScalarFormatter())
#
plt.savefig('tex-outputs/L2.pdf',
        format = 'pdf', 
        bbox_inches='tight')


#tikzplotlib.save("tex-outputs/L2.tex",encoding='utf-8', figure="gcf",extra_axis_parameters= ['height=10cm'])
#,extra_axis_parameters=['yticklabel style={ /pgf/number format/fixed, #/pgf/number format/precision=5}','scaled y ticks=false']) 

# In[10]:


fig, (ax1,ax2) = plt.subplots(
        nrows=2,
        ncols=1,
        sharex=True,
        figsize=set_size(width), 
        )
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
plt.savefig('tex-outputs/ROC.pdf',
        format = 'pdf', 
        bbox_inches='tight')


#tikzplotlib.save("tex-outputs/ROC.tex",figure="gcf",extra_axis_parameters= ['height=10cm'])
#,extra_axis_parameters=['yticklabel style={ /pgf/number format/fixed, #/pgf/number format/precision=5}','scaled y ticks=false']) 

# In[ ]:





# In[8]:


#plt.semilogy(Delta_r,LEE_ROC)
fig, ax = plt.subplots(nrows =4, ncols=1,sharex=True,figsize=set_size(width))

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
plt.savefig('tex-outputs/SourceTermData.pdf')
#tikzplotlib.save("tex-outputs/SourceTermData.tex",extra_axis_parameters=['width=10cm','height=5cm'])


# In[ ]:

#fig, ax = plt.subplots(
#        nrows=1,
#        ncols=1,
#        sharex=True,
#        figsize=(10,4)
#        )
#plt.scatter(
#        cv_wave_data['REAL'],
#        cv_wave_data['IMAG'],
#        marker = '.',
#        s = 1)
#ax.set_ylabel('Imaginary')
#ax.set_ylabel('Real')
#ax.set_title('Convecting Wavenumbers')
#tikzplotlib.save("tex-outputs/cv.waves.scatter.tex",extra_axis_parameters=['width=10cm','height=8cm'])
#
#fig, ax = plt.subplots(
#        nrows=1,
#        ncols=1,
#        sharex=True,
#        figsize=(10,4)
#        )
#plt.scatter(
#        gammas_data['Re{gam/ak}'],
#        gammas_data['Im{gam/ak}'],
#        marker = '.',
#        s = 1)
#tikzplotlib.save("tex-outputs/gammas.scatter.tex",extra_axis_parameters=['width=10cm','height=8cm'])
#
#fig, ax = plt.subplots(
#        nrows=1,
#        ncols=1,
#        sharex=True,
#        figsize=(10,4)
#        )
##print(gam_acc_data.columns)
#plt.scatter(
#        gam_acc_data['Re{gam}/k'],
#        gam_acc_data['Im{gam}/k'],
#        marker = '.',
#        s = 1)
#tikzplotlib.save("tex-outputs/gam.acc.scatter.tex",extra_axis_parameters=['width=10cm','height=8cm'])
#
#fig, ax = plt.subplots(
#        nrows=1,
#        ncols=1,
#        sharex=True,
#        figsize=(10,4)
#        )
#plt.scatter(
#        gam_non_acc_data['Re{gam/ak}'],
#        gam_non_acc_data['Im{gam/ak}'],
#        marker = '.',
#        s = 1)
#tikzplotlib.save("tex-outputs/gam.non.acc.scatter.tex",extra_axis_parameters=['width=10cm','height=8cm'])
