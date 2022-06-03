#!/usr/bin/env python
# coding: utf-8

# In[1]:
import time
import re
import sys
# import tikzplotlib
import glob
import os
import pandas as pd
import numpy as np
import matplotlib as mpl
# mpl.use('pgf')
import matplotlib.ticker as mticker
import plotReportLib
from matplotlib import pyplot as plt
import matplotlib as mpl
from plotReportLib import myfunctions as fcn
from tabulate import tabulate
from texttable import Texttable
import latextable
from my_plot import set_size
start = time.time()
def savefig(*args, **kwargs):
    plt.savefig(*args, **kwargs) 
    plt.close(plt.gcf())

def extract_number(f):
    s = re.findall("\d+$",f)
    return (int(s[0]) if s else -1,f)

plt.style.use('seaborn-whitegrid')
width = 345

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
# input
directories = ['../01-mean-flow/', '../02-method-of-manufactured-solutions/',
        '../04-EVanalysis/']

SourceTermData1_1 = fcn.importPlotData(directories[1] + 'SourceTermData1_0007.dat')
SourceTermData1_2 = fcn.importPlotData(directories[1] + 'SourceTermData1_0013.dat')
SourceTermData1_3 = fcn.importPlotData(directories[1] + 'SourceTermData1_0021.dat')

SourceTermData2_1 = fcn.importPlotData(directories[1] + 'SourceTermData2_0007.dat')
SourceTermData2_2 = fcn.importPlotData(directories[1] + 'SourceTermData2_0013.dat')
SourceTermData2_3 = fcn.importPlotData(directories[1] + 'SourceTermData2_0021.dat')

SourceTermData3_1 = fcn.importPlotData(directories[1] + 'SourceTermData3_0007.dat')
SourceTermData3_2 = fcn.importPlotData(directories[1] + 'SourceTermData3_0013.dat')
SourceTermData3_3 = fcn.importPlotData(directories[1] + 'SourceTermData3_0021.dat')

SourceTermData4_1 = fcn.importPlotData(directories[1] + 'SourceTermData4_0007.dat')
SourceTermData4_2 = fcn.importPlotData(directories[1] + 'SourceTermData4_0013.dat')
SourceTermData4_3 = fcn.importPlotData(directories[1] + 'SourceTermData4_0021.dat')

MeanFlowData1 = fcn.importPlotData(directories[0] + 'mean-flow0007.dat')
MeanFlowData2 = fcn.importPlotData(directories[0] + 'mean-flow0013.dat')
MeanFlowData3 = fcn.importPlotData(directories[0] + 'mean-flow0021.dat')

fig, ax = plt.subplots(
        nrows=1,
        ncols=1,
        sharex=True,
        figsize=set_size(width),
        )

ax.plot(
        MeanFlowData1['radius'],
        MeanFlowData1['M_x'])

ax.plot(
        MeanFlowData2['radius'],
        MeanFlowData2['M_x'])
ax.plot(
        MeanFlowData3['radius'],
        MeanFlowData3['M_theta'])



plt.show()
sys.exit()
fig, axs = plt.subplots(
        nrows=2,
        ncols=2,
        sharex=True,
        figsize=set_size(width),
        )

fig.suptitle('Actual LEE Source Terms')
axs[0,0].plot(
        SourceTermData1_1['radius'],
        SourceTermData1_1['S_actual'])
axs[0,0].plot(
        SourceTermData1_2['radius'],
        SourceTermData1_2['S_actual'])
axs[0,0].plot(
        SourceTermData1_3['radius'],
        SourceTermData1_3['S_actual'])

axs[0,1].plot(
        SourceTermData2_1['radius'],
        SourceTermData2_1['S_actual'])
axs[0,1].plot(
        SourceTermData2_2['radius'],
        SourceTermData2_2['S_actual'])
axs[0,1].plot(
        SourceTermData2_3['radius'],
        SourceTermData2_3['S_actual'])

axs[1,0].plot(
        SourceTermData3_1['radius'],
        SourceTermData3_1['S_actual'])
axs[1,0].plot(
        SourceTermData3_2['radius'],
        SourceTermData3_2['S_actual'])
axs[1,0].plot(
        SourceTermData3_3['radius'],
        SourceTermData3_3['S_actual'],
        )

axs[1,1].plot(
        SourceTermData4_1['radius'],
        SourceTermData4_1['S_actual'])
axs[1,1].plot(
        SourceTermData4_2['radius'],
        SourceTermData4_2['S_actual'])
axs[1,1].plot(
        SourceTermData4_3['radius'],
        SourceTermData4_3['S_actual'],
        )


fig, axs = plt.subplots(
        nrows=2,
        ncols=2,
        sharex=True,
        figsize=set_size(width),
        )

fig.suptitle('Expected LEE Source Terms')
axs[0,0].plot(
        SourceTermData1_1['radius'],
        SourceTermData1_1['S_expected'])
axs[0,0].plot(
        SourceTermData1_2['radius'],
        SourceTermData1_2['S_expected'])
axs[0,0].plot(
        SourceTermData1_3['radius'],
        SourceTermData1_3['S_expected'])

axs[0,1].plot(
        SourceTermData2_1['radius'],
        SourceTermData2_1['S_expected'])
axs[0,1].plot(
        SourceTermData2_2['radius'],
        SourceTermData2_2['S_expected'])
axs[0,1].plot(
        SourceTermData2_3['radius'],
        SourceTermData2_3['S_expected'])

axs[1,0].plot(
        SourceTermData3_1['radius'],
        SourceTermData3_1['S_expected'])
axs[1,0].plot(
        SourceTermData3_2['radius'],
        SourceTermData3_2['S_expected'])
axs[1,0].plot(
        SourceTermData3_3['radius'],
        SourceTermData3_3['S_expected'],
        )

axs[1,1].plot(
        SourceTermData4_1['radius'],
        SourceTermData4_1['S_expected'])
axs[1,1].plot(
        SourceTermData4_2['radius'],
        SourceTermData4_2['S_expected'])
axs[1,1].plot(
        SourceTermData4_3['radius'],
        SourceTermData4_3['S_expected'],
        )

fig, axs = plt.subplots(
        nrows=2,
        ncols=2,
        sharex=True,
        figsize=set_size(width),
        )
axs[0,0].plot(
        SourceTermData1_1['radius'],
        SourceTermData1_1['Error'])
axs[0,0].plot(
        SourceTermData1_2['radius'],
        SourceTermData1_2['Error'])
axs[0,0].plot(
        SourceTermData1_3['radius'],
        SourceTermData1_3['Error'],
        )

axs[0,1].plot(
        SourceTermData2_1['radius'],
        SourceTermData2_1['Error'])
axs[0,1].plot(
        SourceTermData2_2['radius'],
        SourceTermData2_2['Error'])
axs[0,1].plot(
        SourceTermData2_3['radius'],
        SourceTermData2_3['Error'])

axs[1,0].plot(
        SourceTermData3_1['radius'],
        SourceTermData3_1['Error'])
axs[1,0].plot(
        SourceTermData3_2['radius'],
        SourceTermData3_2['Error'])
axs[1,0].plot(
        SourceTermData3_3['radius'],
        SourceTermData3_3['Error'],
        )

axs[1,1].plot(
        SourceTermData4_1['radius'],
        SourceTermData4_1['Error'])
axs[1,1].plot(
        SourceTermData4_2['radius'],
        SourceTermData4_2['Error'])
axs[1,1].plot(
        SourceTermData4_3['radius'],
        SourceTermData4_3['Error'],
        )
#ax.legend()
#directory_32  ='../04-EVanalysis/SWIRLVerification/Table4_3/SecondOrderDiff/32pts/'
#directory_4th_32  ='../04-EVanalysis/SWIRLVerification/Table4_3/FourthOrderDiff/32pts/'
#directory_64  ='../04-EVanalysis/SWIRLVerification/Table4_3/SecondOrderDiff/64pts/'
#directory_4th_64  ='../04-EVanalysis/SWIRLVerification/Table4_3/FourthOrderDiff/64pts/'
#directory_128 ='../04-EVanalysis/SWIRLVerification/Table4_3/SecondOrderDiff/128pts/'
#directory_4th_128 ='../04-EVanalysis/SWIRLVerification/Table4_3/FourthOrderDiff/128pts/'
#directory_256 ='../04-EVanalysis/SWIRLVerification/Table4_3/SecondOrderDiff/256pts/'
#
## 2. Plot Axial Wavenumbers/Eigenvalues
#gam_data = fcn.importPlotData()
#gam_non_acc_data32 = fcn.importPlotData(directory_32+'gam.nonconv.0032')
#gam_non_acc_data_4th_32 = fcn.importPlotData(directory_4th_32+'gam.nonconv.0032')
#gam_non_acc_data64 = fcn.importPlotData(directory_64+'gam.nonconv.0064')
#gam_non_acc_data_4th_64 = fcn.importPlotData(directory_4th_64+'gam.nonconv.0064')
#gam_non_acc_data128 = fcn.importPlotData(directory_128+'gam.nonconv.0128')
#gam_non_acc_data_4th_128 = fcn.importPlotData(directory_4th_128+'gam.nonconv.0128')
#gam_non_acc_data256 = fcn.importPlotData(directory_256+'gam.nonconv.0256')
#
#fig, ax = plt.subplots(
#        nrows=1,
#        ncols=1,
#        sharex=True,
#        figsize=set_size(width),
#        )
#plt.scatter(
#        gam_non_acc_data32['Re{gam/ak}'],
#        gam_non_acc_data32['Im{gam/ak}'],
#        marker = '.',
#        label = '32',
#        s = 2)
#plt.scatter(
#        gam_non_acc_data64['Re{gam/ak}'],
#        gam_non_acc_data64['Im{gam/ak}'],
#        marker = '.',
#        label = '64',
#        s = 2)
#plt.scatter(
#        gam_non_acc_data128['Re{gam/ak}'],
#        gam_non_acc_data128['Im{gam/ak}'],
#        marker = '.',
#        label = '128',
#        s = 2)
#plt.scatter(
#        gam_non_acc_data256['Re{gam/ak}'],
#        gam_non_acc_data256['Im{gam/ak}'],
#        marker = '.',
#        label = '256',
#        s = 2)
#
#ax.legend()
#plt.savefig('tex-outputs/gam.nonconv.scatter_2nd_order_comp.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
#
#fig, ax = plt.subplots(
#        nrows=1,
#        ncols=1,
#        sharex=True,
#        figsize=set_size(width),
#        )
#plt.scatter(
#        gam_non_acc_data_4th_32['Re{gam/ak}'],
#        gam_non_acc_data_4th_32['Im{gam/ak}'],
#        marker = '.',
#        label = '32',
#        s = 2)
#plt.scatter(
#        gam_non_acc_data_4th_64['Re{gam/ak}'],
#        gam_non_acc_data_4th_64['Im{gam/ak}'],
#        marker = '.',
#        label = '64',
#        s = 2)
#plt.scatter(
#        gam_non_acc_data_4th_128['Re{gam/ak}'],
#        gam_non_acc_data_4th_128['Im{gam/ak}'],
#        marker = '.',
#        label = '128',
#        s = 2)
#ax.legend()
#plt.savefig('tex-outputs/gam.nonconv.scatter_4th_order_comp.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
## indices that correspond to the wavenumbers reported in Table 4.3, used visual
## inspection 
#propagation_index_32 = ['0034', '0033', '0031', '0029', '0027', '0025', '0023', 
#                        '0064', '0062', '0060', '0058', '0056', '0054', '0052']
#
#propagation_index_64 = ['0068', '0067', '0065', '0063', '0061', '0059', '0055', '0053', '0051',
#                        '0128', '0126', '0124', '0122', '0120', '0118', '0116', '0114', '0113']
#
#propagation_index_128 = ['0234', '0233', '0231', '0229', '0227', '0223', '0213', '0119','0117' ,
#                         '0256', '0254', '0252', '0250', '0248', '0246', '0244', '0242','0240' ]
#
#propagation_index_256 = ['0494', '0493', '0491', '0489', '0487', '0485', '0481', '0477', '0473', '0451',
#                         '0512', '0510', '0508', '0506', '0504', '0502', '0500', '0498', '0496', '0467']
#
#egvfile_32  = [(directory_32  + 'egv.'+  propagation_index_32[i])  for i,j in (enumerate(propagation_index_32)) ]
#egvfile_64  = [(directory_64  + 'egv.'+  propagation_index_64[i])  for i,j in (enumerate(propagation_index_64)) ]
#egvfile_128 = [(directory_128 + 'egv.'+  propagation_index_128[i]) for i,j in (enumerate(propagation_index_128))]
#egvfile_256 = [(directory_256 + 'egv.'+  propagation_index_256[i]) for i,j in (enumerate(propagation_index_256))]
#
#mode_data_32   = [(fcn.importPlotData(str(egvfile_32[i])))  for i,j in (enumerate(propagation_index_32))]
#mode_data_64   = [(fcn.importPlotData(str(egvfile_64[i])))  for i,j in (enumerate(propagation_index_64))]
#mode_data_128  = [(fcn.importPlotData(str(egvfile_128[i]))) for i,j in (enumerate(propagation_index_128))]
#mode_data_256  = [(fcn.importPlotData(str(egvfile_256[i]))) for i,j in (enumerate(propagation_index_256))]
#
#markers = ['o-', '+-', '--', '-', 'o-', '.', 'x', 'X', 'D', '|']
#y_str = ['p_no_phase[Re]', 'p_no_phase[Im]']
#
## 2. Plot Pressure Mode Shapes/Eigenvectors
#Tot = 10
#Cols = 2
#
#Rows = Tot // Cols
#Rows += Tot % Cols
#Position = range(1,Tot + 1)
#
#fig = plt.figure(constrained_layout=False, figsize=set_size(2*width))
#fig.suptitle('Propagating Modes [Real]')
#s = fig.add_gridspec(Rows, Cols,hspace=0 )
#
#for k in range(Tot):
#    if k <= 6:
#        ax = fig.add_subplot(s[k])
#        ax.plot( 
#                mode_data_32[k]['Rad'],
#                mode_data_32[k][y_str[0]] , 
#                markers[0%10],
#                markersize = 2 ,
#                label = '32')
#        ax.plot( 
#                mode_data_64[k]['Rad'],
#                mode_data_64[k][y_str[0]] , 
#                markers[1%10],
#                markersize = 2 ,
#                label = '64',)
#        ax.plot( 
#                mode_data_128[k]['Rad'],
#                mode_data_128[k][y_str[0]] , 
#                markers[2%10],
#                label = '128')
#        ax.plot( 
#                mode_data_256[k]['Rad'],
#                mode_data_256[k][y_str[0]], 
#                markers[3%10],
#                label = '256')
#        ax.set_ylabel('Mode ' + str(k))
#        ax.legend()
#    elif k <= 8:
#        ax = fig.add_subplot(s[k],sharex=ax)
#        ax.plot( 
#                mode_data_64[k]['Rad'],
#                mode_data_64[k][y_str[0]] , 
#                markers[0%10],
#                markersize = 2 ,
#                label = '64')
#        ax.plot( 
#                mode_data_128[k]['Rad'],
#                mode_data_128[k][y_str[0]] , 
#                markers[1%10],
#                markersize = 2 ,
#                label = '128')
#        ax.plot( 
#                mode_data_256[k]['Rad'],
#                mode_data_256[k][y_str[0]], 
#                markers[2%10],
#                markersize = 2 ,
#                label = '256')
#        ax.set_ylabel('Mode ' + str(k))
#        ax.legend()
#    elif k<=9:
#        ax = fig.add_subplot(s[k])
#        ax.plot( 
#                mode_data_256[k]['Rad'],
#                mode_data_256[k][y_str[0]], 
#                markers[0%10],
#                markersize = 2 ,
#                label = '256')
#        ax.set_ylabel('Mode ' + str(k))
#        ax.legend()
#
#plt.savefig('tex-outputs/egv_prop_re.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
#
#Rows = Tot // Cols
#Rows += Tot % Cols
#Position = range(1,Tot + 1)
#fig = plt.figure(constrained_layout=False, figsize=set_size(2*width))
#fig.suptitle('Decaying Modes [Real]')
#
#s = fig.add_gridspec(Rows, Cols,hspace=0)
#
#for k in range(Tot):
#    if k <= 6:
#        ax = fig.add_subplot(s[k])
#        ax.plot( 
#                mode_data_32[k+7]['Rad'],
#                mode_data_32[k+7][y_str[0]] , 
#                markers[0%10],
#                markersize = 2, 
#                label = '32')
#        ax.plot( 
#                mode_data_64[k+9]['Rad'],
#                mode_data_64[k+9][y_str[0]] , 
#                markers[1%10],
#                markersize = 2, 
#                label = '64',)
#        ax.plot( 
#                mode_data_128[k+9]['Rad'],
#                mode_data_128[k+9][y_str[0]] , 
#                markers[2%10],
#                markersize = 2, 
#                label = '128')
#        ax.plot( 
#                mode_data_256[k+10]['Rad'],
#                mode_data_256[k+10][y_str[0]], 
#                markers[3%10],
#                markersize = 2, 
#                label = '256')
#        ax.set_ylabel('Mode -' + str(k))
#        ax.legend()
#    elif k <= 8:
#        ax = fig.add_subplot(s[k])
#        ax.plot( 
#                mode_data_64[k+9]['Rad'],
#                mode_data_64[k+9][y_str[0]] , 
#                markers[0%10],
#                markersize = 2, 
#                label = '64',)
#        ax.plot( 
#                mode_data_128[k+9]['Rad'],
#                mode_data_128[k+9][y_str[0]] , 
#                markers[1%10],
#                markersize = 2, 
#                label = '128')
#        ax.plot( 
#                mode_data_256[k+10]['Rad'],
#                mode_data_256[k+10][y_str[0]], 
#                markers[2%10],
#                markersize = 2, 
#                label = '256')
#        ax.set_ylabel('Mode -' + str(k))
#        ax.legend()
#    elif k<=9:
#        ax = fig.add_subplot(s[k])
#        ax.plot( 
#                mode_data_256[k+10]['Rad'],
#                mode_data_256[k+10][y_str[0]], 
#                markers[0%10],
#                markersize = 2, 
#                label = '256')
#        ax.set_ylabel('Mode -' + str(k))
#        ax.legend()
#
#plt.savefig('tex-outputs/egv_decay_re.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
#
#
#fig = plt.figure(constrained_layout=False, figsize=set_size(2*width))
#fig.suptitle('Propagating Modes [Imaginary]')
#s = fig.add_gridspec(Rows, Cols,hspace=0 )
#
#for k in range(Tot):
#    if k <= 6:
#        ax = fig.add_subplot(s[k])
#        ax.plot( 
#                mode_data_32[k]['Rad'],
#                mode_data_32[k][y_str[1]] , 
#                markers[0%10],
#                markersize = 2 ,
#                label = '32')
#        ax.plot( 
#                mode_data_64[k]['Rad'],
#                mode_data_64[k][y_str[1]] , 
#                markers[1%10],
#                markersize = 2 ,
#                label = '64',)
#        ax.plot( 
#                mode_data_128[k]['Rad'],
#                mode_data_128[k][y_str[1]] , 
#                markers[2%10],
#                label = '128')
#        ax.plot( 
#                mode_data_256[k]['Rad'],
#                mode_data_256[k][y_str[1]], 
#                markers[3%10],
#                label = '256')
#        ax.set_ylabel('Mode ' + str(k))
#        ax.legend()
#    elif k <= 8:
#        ax = fig.add_subplot(s[k],sharex=ax)
#        ax.plot( 
#                mode_data_64[k]['Rad'],
#                mode_data_64[k][y_str[1]] , 
#                markers[0%10],
#                markersize = 2 ,
#                label = '64')
#        ax.plot( 
#                mode_data_128[k]['Rad'],
#                mode_data_128[k][y_str[1]] , 
#                markers[1%10],
#                markersize = 2 ,
#                label = '128')
#        ax.plot( 
#                mode_data_256[k]['Rad'],
#                mode_data_256[k][y_str[1]], 
#                markers[2%10],
#                markersize = 2 ,
#                label = '256')
#        ax.set_ylabel('Mode ' + str(k))
#        ax.legend()
#    elif k<=9:
#        ax = fig.add_subplot(s[k])
#        ax.plot( 
#                mode_data_256[k]['Rad'],
#                mode_data_256[k][y_str[1]], 
#                markers[0%10],
#                markersize = 2 ,
#                label = '256')
#        ax.set_ylabel('Mode ' + str(k))
#        ax.legend()
#
#plt.savefig('tex-outputs/egv_prop_im.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
#
#
#Rows = Tot // Cols
#Rows += Tot % Cols
#Position = range(1,Tot + 1)
#fig = plt.figure(constrained_layout=False, figsize=set_size(2*width))
#fig.suptitle('Decaying Modes [Imaginary]')
#
#s = fig.add_gridspec(Rows, Cols,hspace=0)
#
#for k in range(Tot):
#    if k <= 6:
#        ax = fig.add_subplot(s[k])
#        ax.plot( 
#                mode_data_32[k+7]['Rad'],
#                mode_data_32[k+7][y_str[1]] , 
#                markers[0%10],
#                markersize = 2, 
#                label = '32')
#        ax.plot( 
#                mode_data_64[k+9]['Rad'],
#                mode_data_64[k+9][y_str[1]] , 
#                markers[1%10],
#                markersize = 2, 
#                label = '64',)
#        ax.plot( 
#                mode_data_128[k+9]['Rad'],
#                mode_data_128[k+9][y_str[1]] , 
#                markers[2%10],
#                markersize = 2, 
#                label = '128')
#        ax.plot( 
#                mode_data_256[k+10]['Rad'],
#                mode_data_256[k+10][y_str[1]], 
#                markers[3%10],
#                markersize = 2, 
#                label = '256')
#        ax.set_ylabel('Mode -' + str(k))
#        ax.legend()
#    elif k <= 8:
#        ax = fig.add_subplot(s[k])
#        ax.plot( 
#                mode_data_64[k+9]['Rad'],
#                mode_data_64[k+9][y_str[1]] , 
#                markers[0%10],
#                markersize = 2, 
#                label = '64',)
#        ax.plot( 
#                mode_data_128[k+9]['Rad'],
#                mode_data_128[k+9][y_str[1]] , 
#                markers[1%10],
#                markersize = 2, 
#                label = '128')
#        ax.plot( 
#                mode_data_256[k+10]['Rad'],
#                mode_data_256[k+10][y_str[1]], 
#                markers[2%10],
#                markersize = 2, 
#                label = '256')
#        ax.set_ylabel('Mode -' + str(k))
#        ax.legend()
#    elif k<=9:
#        ax = fig.add_subplot(s[k])
#        ax.plot( 
#                mode_data_256[k+10]['Rad'],
#                mode_data_256[k+10][y_str[1]], 
#                markers[0%10],
#                markersize = 2, 
#                label = '256')
#        ax.set_ylabel('Mode -' + str(k))
#        ax.legend()
#
#plt.savefig('tex-outputs/egv_decay_im.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
#
## plt.show()
#end = time.time()
#print(end-start)
#
#sys.exit()
#Tot = 7
#Cols = 2
#
#Rows = Tot // Cols
#Rows += Tot % Cols
#Position = range(1,Tot + 1)
#fig = plt.figure(constrained_layout=False, figsize=set_size(width))
## fig = plt.figure(constrained_layout=False, figsize=set_size(2*width, 400))
#fig.suptitle('Decaying Modes [Real]')
#s = fig.add_gridspec(Rows, Cols)#, width_ratios = [1, 1, 1], height_ratios = [1, 1, 1])
#for k in range(Tot):
#    ax = fig.add_subplot(Rows,Cols,Position[k])
#    ax.plot( 
#             mode_data_32[k+7]['Rad'],
#             mode_data_32[k+7][y_str[0]] , 
#             markers[1%10],
#             label = '32')
#    ax.plot( 
#            mode_data_64[k+9]['Rad'],
#            mode_data_64[k+9][y_str[0]] , 
#            markers[2%10],
#            label = '64',)
#    ax.plot( 
#            mode_data_128[k+9]['Rad'],
#            mode_data_128[k+9][y_str[0]] , 
#            markers[3%10],
#            label = '128')
#    ax.plot( 
#            mode_data_256[k+10]['Rad'],
#            mode_data_256[k+10][y_str[0]], 
#            markers[4%10],
#            label = '256')
#    ax.set_ylabel('Mode -' + str(k))
#    ax.legend()
#
#Rows = Tot // Cols
#Rows += Tot % Cols
#Position = range(1,Tot + 1)
#
#
#fig = plt.figure(constrained_layout=False, figsize=set_size(width))
## fig = plt.figure(constrained_layout=False, figsize=set_size(2*width,400))
#fig.suptitle('Propagating Modes [Imaginary]')
#s = fig.add_gridspec(Rows, Cols)#, width_ratios = [1, 1, 1], height_ratios = [1, 1, 1])
## fig = plt.figure(figsize=set_size(2*width))
#for k in range(Tot):
#    ax = fig.add_subplot(Rows,Cols,Position[k])
#    ax.plot( 
#            mode_data_32[k]['Rad'],
#            mode_data_32[k][y_str[1]] , 
#            markers[1%10],
#            label = '32')
#    ax.plot( 
#            mode_data_64[k]['Rad'],
#            mode_data_64[k][y_str[1]] , 
#            markers[2%10],
#            label = '64',)
#    ax.plot( 
#            mode_data_128[k]['Rad'],
#            mode_data_128[k][y_str[1]] , 
#            markers[3%10],
#            label = '128')
#    ax.plot( 
#            mode_data_256[k]['Rad'],
#            mode_data_256[k][y_str[1]], 
#            markers[4%10],
#            label = '256')
#    ax.set_label('Mode -' + str(k))
#    ax.legend()
#
#plt.savefig('tex-outputs/egv_prop_im.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
#
#fig = plt.figure(constrained_layout=False, figsize=set_size(width))
## fig = plt.figure(constrained_layout=False, figsize=set_size(2*width,400))
#fig.suptitle('Decaying Modes [Imaginary]')
#s = fig.add_gridspec(Rows, Cols)#, width_ratios = [1, 1, 1], height_ratios = [1, 1, 1])
## fig = plt.figure(figsize=set_size(2*width))
#for k in range(Tot):
#    ax = fig.add_subplot(Rows,Cols,Position[k])
#    ax.plot( 
#            mode_data_32[k+7]['Rad'],
#            mode_data_32[k+7][y_str[1]] , 
#            markers[1%10],
#            label = '32')
#    ax.plot( 
#            mode_data_64[k+9]['Rad'],
#            mode_data_64[k+9][y_str[1]] , 
#            markers[2%10],
#            label = '64',)
#    ax.plot( 
#            mode_data_128[k+9]['Rad'],
#            mode_data_128[k+9][y_str[1]] , 
#            markers[3%10],
#            label = '128')
#    ax.plot( 
#            mode_data_256[k+10]['Rad'],
#            mode_data_256[k+10][y_str[1]], 
#            markers[4%10],
#            label = '256')
#    ax.set_title('Mode -' + str(k))
#    ax.legend()
#
#plt.savefig('tex-outputs/egv_decay_im.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
#
# Tot = 9 
# Cols = 2

# Rows = Tot // Cols
# Rows += Tot % Cols
# Position = range(1,Tot + 1)

# fig = plt.figure(constrained_layout=False, figsize=set_size(2*width,400))
# fig.suptitle('First 7 Propagating Modes [Imaginary]')
# s = fig.add_gridspec(Rows, Cols)#, width_ratios = [1, 1, 1], height_ratios = [1, 1, 1])

# for k in range(Tot):
#     ax = fig.add_subplot(Rows,Cols,Position[k])
#     ax.plot( 
#             mode_data_64[k]['Rad'],
#             mode_data_64[k][y_str[0]] , 
#             markers[2%10],
#             label = '64',)
#     ax.plot( 
#             mode_data_128[k]['Rad'],
#             mode_data_128[k][y_str[0]] , 
#             markers[3%10],
#             label = '128')
#     ax.plot( 
#             mode_data_256[k]['Rad'],
#             mode_data_256[k][y_str[0]], 
#             markers[4%10],
#             label = '256')
#     ax.set_title('Mode ' + str(k))
#     ax.legend()
#
#Tot = 3 
#Cols = 2
#
#Rows = Tot // Cols
#Rows += Tot % Cols
#Position = range(1,Tot + 1)
#
#fig = plt.figure(constrained_layout=False, figsize=set_size(2*width,400))
#fig.suptitle('Last Propagating Modes [Re]')
#s = fig.add_gridspec(Rows, Cols)#, width_ratios = [1, 1, 1], height_ratios = [1, 1, 1])
#
#for k in range(Tot):
#    ax = fig.add_subplot(Rows,Cols,Position[k])
#    ax.plot( 
#            mode_data_64[k+9]['Rad'],
#            mode_data_64[k+9][y_str[1]] , 
#            markers[2%10],
#            label = '64',)
#    ax.plot( 
#            mode_data_128[k+9]['Rad'],
#            mode_data_128[k+9][y_str[1]] , 
#            markers[3%10],
#            label = '128')
#    ax.plot( 
#            mode_data_256[k+9]['Rad'],
#            mode_data_256[k+9][y_str[1]], 
#            markers[4%10],
#            label = '256')
#    ax.set_title('Mode ' + str(k+9))
#    ax.legend()
#
# Tot = 11 
# Cols = 2

# Rows = Tot // Cols
# Rows += Tot % Cols
# Position = range(1,Tot + 1)

# fig = plt.figure(constrained_layout=False, figsize=set_size(2*width,400))
# fig.suptitle('First 9 Decaying Modes')
# s = fig.add_gridspec(Rows, Cols)#, width_ratios = [1, 1, 1], height_ratios = [1, 1, 1])
# # fig = plt.figure(figsize=set_size(2*width))
# for k in range(Tot):
#     ax = fig.add_subplot(Rows,Cols,Position[k])
#     ax.plot( 
#             mode_data_64[k]['Rad'],
#             mode_data_64[k][y_str[1]] , 
#             markers[2%10],
#             label = '64',)
#     ax.plot( 
#             mode_data_128[k]['Rad'],
#             mode_data_128[k][y_str[1]] , 
#             markers[3%10],
#             label = '128')
#     ax.plot( 
#             mode_data_256[k]['Rad'],
#             mode_data_256[k][y_str[1]], 
#             markers[4%10],
#             label = '256')

# plt.show()
#
# Tot = 9  
# Cols = 2

# Rows = Tot // Cols
# Rows += Tot % Cols
# Position = range(1,Tot + 1)

# fig = plt.figure(constrained_layout=False, figsize=set_size(2*width,400))
# s = fig.add_gridspec(Rows, Cols)#, width_ratios = [1, 1, 1], height_ratios = [1, 1, 1])
# # fig = plt.figure(figsize=set_size(2*width))
# for k in range(Tot):
#     ax = fig.add_subplot(Rows,Cols,Position[k])
#     ax.plot( 
#             mode_data_128[k]['Rad'],
#             mode_data_128[k][y_str[0]] , 
#             markers[3%10],
#             label = '128')
#     ax.plot( 
#             mode_data_256[k]['Rad'],
#             mode_data_256[k][y_str[0]], 
#             markers[4%10],
#             label = '256')
# plt.show()
#for row in range(3):
#    for col in range(3):
#        ax = fig.add_subplot(s[row, col])
#
#
#fig = plt.figure( figsize=set_size(width) )
#gs  = fig.add_gridspec(2, hspace=0)
#axs = gs.subplots(sharex=True, sharey=False)
#m   = 1
#
#axs[0].plot( 
#        mode_data_32[m]['Rad'],
#        mode_data_32[m][y_str[0]] , 
#        markers[1%10],
#        label = '32')
#
#axs[0].plot( 
#        mode_data_64[m]['Rad'],
#        mode_data_64[m][y_str[0]] , 
#        markers[2%10],
#        label = '64',)
#axs[0].plot( 
#        mode_data_128[m]['Rad'],
#        mode_data_128[m][y_str[0]] , 
#        markers[3%10],
#        label = '128')
#axs[0].plot( 
#        mode_data_256[m]['Rad'],
#        mode_data_256[m][y_str[0]], 
#        markers[4%10],
#        label = '256')
#axs[1].plot( 
#        mode_data_32[m]['Rad'],
#        mode_data_32[m][y_str[1]] , 
#        markers[1%10],
#        label = '32')
#
#axs[1].plot( 
#        mode_data_64[m]['Rad'],
#        mode_data_64[m][y_str[1]] , 
#        markers[2%10],
#        label = '64',)
#axs[1].plot( 
#        mode_data_128[m]['Rad'],
#        mode_data_128[m][y_str[1]] , 
#        markers[3%10],
#        label = '128')
#axs[1].plot( 
#        mode_data_256[m]['Rad'],
#        mode_data_256[m][y_str[1]], 
#        markers[4%10],
#        label = '256')
#plt.show()
#sys.exit()
#ax1.plot(
#        mode_data_re1_64['Rad'],
#        mode_data_re1_64['p_no_phase'] ,
#        markers[2%10]         , 
#        label = '64'       )
#
#ax1.plot(
#        mode_data_re1_128['Rad'],
#        mode_data_re1_128['p_no_phase']  , 
#        markers[3%10],
#        label = '128')
#
#ax1.plot(
#        mode_data_re1_256['Rad'],
#        mode_data_re1_256['p_no_phase']  , 
#        markers[4%10],
#        label = '256'       )
#        
#ax2.plot( 
#        mode_data_im1_32['Rad'],
#        mode_data_im1_32['p_no_phase'] ,
#        markers[1%10]       ,
#        mode_data_im1_64['Rad'],
#        mode_data_im1_64['p_no_phase'] ,
#        markers[2%10]       ,
#        mode_data_im1_128['Rad'],
#        mode_data_im1_128['p_no_phase']  , 
#        markers[3%10],
#        mode_data_im1_256['Rad'],
#        mode_data_im1_256['p_no_phase']  , 
#        markers[4%10]          )
#        
#ax1.set_ylabel('[Real]')
#ax2.set_ylabel('[Imaginary]')
#ax2.set_xlabel('Radius')
#ax1.legend(loc="upper right") 
#plt.savefig('tex-outputs/egv1.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
## gamma +1
#fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))
#
#ax1.plot( 
#        mode_data_re2_32['Rad'],
#        mode_data_re2_32['p_no_phase'] , 
#        markers[1%10],
#        label = '32')
#
#ax1.plot( 
#        mode_data_re2_64['Rad'],
#        mode_data_re2_64['p_no_phase'] ,
#        markers[2%10]         , 
#        label = '64'       )
#
#ax1.plot(
#        mode_data_re2_128['Rad'],
#        mode_data_re2_128['p_no_phase']  , 
#        markers[3%10],
#        label = '128')
#
#ax1.plot( 
#        mode_data_re2_256['Rad'],
#        mode_data_re2_256['p_no_phase']  , 
#        markers[4%10],
#        label = '256'       )
#        
#ax2.plot( 
#        mode_data_im2_32['Rad'],
#        mode_data_im2_32['p_no_phase'] ,
#        markers[1%10]       ,
#        # label = '32'        ,
#        mode_data_im2_64['Rad'],
#        mode_data_im2_64['p_no_phase'] ,
#        markers[2%10]       ,
#        #label = '64'        ,
#        mode_data_im2_128['Rad'],
#        mode_data_im2_128['p_no_phase']  , 
#        markers[3%10],
#        #label = '128'        ,
#        mode_data_im2_256['Rad'],
#        mode_data_im2_256['p_no_phase']  , 
#        markers[4%10]          )
#
#ax1.set_ylabel('[Real]')
#ax2.set_ylabel('[Imaginary]')
#ax2.set_xlabel('Radius')
#ax1.legend()
#        
#plt.savefig('tex-outputs/egv2.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
#
## gamma +2
#fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))
#
#ax1.plot( 
#        mode_data_re3_32['Rad'],
#        mode_data_re3_32['p_no_phase'] , 
#        markers[1%10],
#        label = '32')
#
#ax1.plot( 
#        mode_data_re3_64['Rad'],
#        mode_data_re3_64['p_no_phase'] ,
#        markers[2%10]         , 
#        label = '64'       )
#
#ax1.plot(
#        mode_data_re3_128['Rad'],
#        mode_data_re3_128['p_no_phase']  , 
#        markers[3%10],
#        label = '128')
#
#ax1.plot( 
#        mode_data_re3_256['Rad'],
#        mode_data_re3_256['p_no_phase'] , 
#        markers[4%10],
#        label = '256')
#
#ax2.plot( 
#        mode_data_im3_32['Rad'],
#        mode_data_im3_32['p_no_phase'] ,
#        # label = '32'        ,
#        markers[1%10]       ,
#        mode_data_im3_64['Rad'],
#        mode_data_im3_64['p_no_phase'] ,
#        # label = '64'        ,
#        markers[2%10]       ,
#        mode_data_im3_128['Rad'],
#        mode_data_im3_128['p_no_phase']  , 
#        # label = '128'        ,
#        markers[3%10],
#        mode_data_im3_256['Rad'],
#        mode_data_im3_256['p_no_phase']  , 
#        # label = '256'        ,
#        markers[4%10])
#        
#
#ax1.set_ylabel('[Real]')
#ax2.set_ylabel('[Imaginary]')
#ax2.set_xlabel('Radius')
#ax1.legend(loc="upper left") 
#
#plt.savefig(
#        'tex-outputs/egv3.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
#
#
## gamma +3
#fig, (ax1, ax2) = plt.subplots(
#        2, 1, figsize=set_size(width))
#
#ax1.plot( 
#        mode_data_re4_32['Rad'],
#        mode_data_re4_32['p_no_phase'] , 
#        markers[1%10],
#        label = '32')
#ax1.plot(
#        mode_data_re4_64['Rad'],
#        mode_data_re4_64['p_no_phase'] ,
#        markers[2%10]         , 
#        label = '64'       )
#ax1.plot(
#        mode_data_re4_128['Rad'],
#        mode_data_re4_128['p_no_phase']  , 
#        markers[3%10],
#        label = '128')
#ax1.plot(
#        mode_data_re4_256['Rad'],
#        mode_data_re4_256['p_no_phase']  , 
#        markers[4%10],
#        label = '256'       )
#        
#ax2.plot( 
#        mode_data_im4_32['Rad'],
#        mode_data_im4_32['p_no_phase'] ,
#        markers[1%10]       ,
#        # label = '32'        ,
#        mode_data_im4_64['Rad'],
#        mode_data_im4_64['p_no_phase'] ,
#        markers[2%10]       ,
#        #label = '64'        ,
#        mode_data_im4_128['Rad'],
#        mode_data_im4_128['p_no_phase']  , 
#        markers[3%10],
#        #label = '128'        ,
#        mode_data_im4_256['Rad'],
#        mode_data_im4_256['p_no_phase']  , 
#        markers[4%10]                  )
#        
#ax1.set_ylabel('[Real]')
#ax2.set_ylabel('[Imaginary]')
#ax2.set_xlabel('Radius')
#ax1.legend(loc="upper right") 
#plt.savefig('tex-outputs/egv4.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
##----
#
## gamma +4
#fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))
#
#
#ax1.plot( 
#        mode_data_re5_32['Rad'],
#        mode_data_re5_32['p_no_phase'] , 
#        markers[1%10],
#        label = '32')
#
#ax1.plot(
#        mode_data_re5_64['Rad'],
#        mode_data_re5_64['p_no_phase'] ,
#        markers[2%10]         , 
#        label = '64'       )
#ax1.plot(
#        mode_data_re5_128['Rad'],
#        mode_data_re5_128['p_no_phase']  , 
#        markers[3%10],
#        label = '128')
#ax1.plot(
#        mode_data_re5_256['Rad'],
#        mode_data_re5_256['p_no_phase']  , 
#        markers[4%10],
#        label = '256'       )
#ax2.plot( 
#        mode_data_im5_32['Rad'],
#        mode_data_im5_32['p_no_phase'] ,
#        markers[1%10]       ,
#        mode_data_im5_64['Rad'],
#        mode_data_im5_64['p_no_phase'] ,
#        markers[2%10]       ,
#        mode_data_im5_128['Rad'],
#        mode_data_im5_128['p_no_phase']  , 
#        markers[3%10],
#        mode_data_im5_256['Rad'],
#        mode_data_im5_256['p_no_phase']  , 
#        markers[4%10]          )
#
#ax1.set_ylabel('[Real]')
#ax2.set_ylabel('[Imaginary]')
#ax2.set_xlabel('Radius')
#ax1.legend(loc="upper right") 
#plt.savefig('tex-outputs/egv5.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
#
## gamma +5
#fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))
#ax1.plot( 
#        mode_data_re6_32['Rad'],
#        mode_data_re6_32['p_no_phase'] , 
#        markers[1%10],
#        label = '32')
#
#ax1.plot(
#        mode_data_re6_64['Rad'],
#        mode_data_re6_64['p_no_phase'] ,
#        markers[2%10]         , 
#        label = '64'       )
#ax1.plot(
#        mode_data_re6_128['Rad'],
#        mode_data_re6_128['p_no_phase']  , 
#        markers[3%10],
#        label = '128')
#ax1.plot(
#        mode_data_re6_256['Rad'],
#        mode_data_re6_256['p_no_phase']  , 
#        markers[4%10],
#        label = '256'       )
#ax2.plot( 
#        mode_data_im6_32['Rad'],
#        mode_data_im6_32['p_no_phase'] ,
#        markers[1%10]       ,
#        mode_data_im6_64['Rad'],
#        mode_data_im6_64['p_no_phase'] ,
#        markers[2%10]       ,
#        mode_data_im6_128['Rad'],
#        mode_data_im6_128['p_no_phase']  , 
#        markers[3%10],
#        mode_data_im6_256['Rad'],
#        mode_data_im6_256['p_no_phase']  , 
#        markers[4%10]         ) 
#ax1.set_ylabel('[Real]')
#ax2.set_ylabel('[Imaginary]')
#ax2.set_xlabel('Radius')
#ax1.legend(loc="upper right") 
#plt.savefig('tex-outputs/egv6.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
#
## gamma +6
#fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))
#ax1.plot( 
#        mode_data_re7_32['Rad'],
#        mode_data_re7_32['p_no_phase'] , 
#        markers[1%10],
#        label = '32')
#
#ax1.plot(
#        mode_data_re7_64['Rad'],
#        mode_data_re7_64['p_no_phase'] ,
#        markers[2%10]         , 
#        label = '64'       )
#ax1.plot(
#        mode_data_re7_128['Rad'],
#        mode_data_re7_128['p_no_phase']  , 
#        markers[3%10],
#        label = '128')
#ax1.plot(
#        mode_data_re7_256['Rad'],
#        mode_data_re7_256['p_no_phase']  , 
#        markers[4%10],
#        label = '256'       )
#ax2.plot( 
#        mode_data_im7_32['Rad'],
#        mode_data_im7_32['p_no_phase'] ,
#        markers[1%10]       ,
#        mode_data_im7_64['Rad'],
#        mode_data_im7_64['p_no_phase'] ,
#        markers[2%10]       ,
#        mode_data_im7_128['Rad'],
#        mode_data_im7_128['p_no_phase']  , 
#        markers[3%10],
#        mode_data_im7_256['Rad'],
#        mode_data_im7_256['p_no_phase']  , 
#        markers[4%10]         )
#ax1.set_ylabel('[Real]')
#ax2.set_ylabel('[Imaginary]')
#ax2.set_xlabel('Radius')
#ax1.legend(loc="upper right") 
#plt.savefig('tex-outputs/egv7.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
#
## gamma +7
#fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))
#ax1.plot(
#        mode_data_re8_64['Rad'],
#        mode_data_re8_64['p_no_phase'] ,
#        markers[2%10]         , 
#        label = '64'       )
#ax1.plot(
#        mode_data_re8_128['Rad'],
#        mode_data_re8_128['p_no_phase']  , 
#        markers[3%10],
#        label = '128')
#ax1.plot(
#        mode_data_re8_256['Rad'],
#        mode_data_re8_256['p_no_phase']  , 
#        markers[4%10],
#        label = '256'       )
#ax2.plot( 
#        mode_data_im8_64['Rad'],
#        mode_data_im8_64['p_no_phase'] ,
#        markers[2%10]       ,
#        mode_data_im8_128['Rad'],
#        mode_data_im8_128['p_no_phase']  , 
#        markers[3%10],
#        mode_data_im8_256['Rad'],
#        mode_data_im8_256['p_no_phase']  , 
#        markers[4%10]          )
#ax1.set_ylabel('[Real]')
#ax2.set_ylabel('[Imaginary]')
#ax2.set_xlabel('Radius')
#ax1.legend(loc="upper right") 
#plt.savefig('tex-outputs/egv8.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
#
## gamma +8
#fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))
#ax1.plot(
#        mode_data_re9_64['Rad'],
#        mode_data_re9_64['p_no_phase'] ,
#        markers[2%10]         , 
#        label = '64'       )
#ax1.plot(
#        mode_data_re9_128['Rad'],
#        mode_data_re9_128['p_no_phase']  , 
#        markers[3%10],
#        label = '128')
#ax1.plot(
#        mode_data_re9_256['Rad'],
#        mode_data_re9_256['p_no_phase']  , 
#        markers[4%10],
#        label = '256'       )
#
#ax2.plot( 
#        mode_data_im9_64['Rad'],
#        mode_data_im9_64['p_no_phase'] ,
#        markers[2%10]       ,
#        mode_data_im9_128['Rad'],
#        mode_data_im9_128['p_no_phase']  , 
#        markers[3%10],
#        mode_data_im9_256['Rad'],
#        mode_data_im9_256['p_no_phase']  , 
#        markers[4%10]          )
#
#ax1.set_ylabel('[Real]')
#ax2.set_ylabel('[Imaginary]')
#ax2.set_xlabel('Radius')
#ax1.legend(loc="upper right") 
#plt.savefig('tex-outputs/egv9.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
#
#
## gamma -0
#fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))
#
#ax1.plot( 
#        mode_data_re_up1_32['Rad'],
#        mode_data_re_up1_32['p_no_phase'] , 
#        markers[1%10],
#        label = '32')
#
#ax1.plot(
#        mode_data_re_up1_64['Rad'],
#        mode_data_re_up1_64['p_no_phase'] ,
#        markers[2%10]         , 
#        label = '64'       )
#
#ax1.plot(
#        mode_data_re_up1_128['Rad'],
#        mode_data_re_up1_128['p_no_phase']  , 
#        markers[3%10],
#        label = '128')
#
#ax1.plot(
#        mode_data_re_up1_256['Rad'],
#        mode_data_re_up1_256['p_no_phase']  , 
#        markers[4%10],
#        label = '256'       )
#        
#ax2.plot( 
#        mode_data_im_up1_32['Rad'],
#        mode_data_im_up1_32['p_no_phase'] ,
#        markers[1%10]       ,
#        mode_data_im_up1_64['Rad'],
#        mode_data_im_up1_64['p_no_phase'] ,
#        markers[2%10]       ,
#        mode_data_im_up1_128['Rad'],
#        mode_data_im_up1_128['p_no_phase']  , 
#        markers[3%10],
#        mode_data_im_up1_256['Rad'],
#        mode_data_im_up1_256['p_no_phase']  , 
#        markers[4%10]          )
#        
#ax1.set_ylabel('[Real]')
#ax2.set_ylabel('[Imaginary]')
#ax2.set_xlabel('Radius')
#ax1.legend(loc="upper right") 
#plt.savefig('tex-outputs/egv_m1.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
#
#
## gamma -1
#fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))
#
#ax1.plot( 
#        mode_data_re_up2_32['Rad'],
#        mode_data_re_up2_32['p_no_phase'] , 
#        markers[1%10],
#        label = '32')
#
#ax1.plot(
#        mode_data_re_up2_64['Rad'],
#        mode_data_re_up2_64['p_no_phase'] ,
#        markers[2%10]         , 
#        label = '64'       )
#
#ax1.plot(
#        mode_data_re_up2_128['Rad'],
#        mode_data_re_up2_128['p_no_phase']  , 
#        markers[3%10],
#        label = '128')
#
#ax1.plot(
#        mode_data_re_up2_256['Rad'],
#        mode_data_re_up2_256['p_no_phase']  , 
#        markers[4%10],
#        label = '256'       )
#        
#ax2.plot( 
#        mode_data_im_up2_32['Rad'],
#        mode_data_im_up2_32['p_no_phase'] ,
#        markers[1%10]       ,
#        mode_data_im_up2_64['Rad'],
#        mode_data_im_up2_64['p_no_phase'] ,
#        markers[2%10]       ,
#        mode_data_im_up2_128['Rad'],
#        mode_data_im_up2_128['p_no_phase']  , 
#        markers[3%10],
#        mode_data_im_up2_256['Rad'],
#        mode_data_im_up2_256['p_no_phase']  , 
#        markers[4%10]          )
#        
#ax1.set_ylabel('[Real]')
#ax2.set_ylabel('[Imaginary]')
#ax2.set_xlabel('Radius')
#ax1.legend(loc="upper right") 
#plt.savefig('tex-outputs/egv_m2.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
#
#
## gamma -2
#fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))
#
#ax1.plot( 
#        mode_data_re_up3_32['Rad'],
#        mode_data_re_up3_32['p_no_phase'] , 
#        markers[1%10],
#        label = '32')
#
#ax1.plot(
#        mode_data_re_up3_64['Rad'],
#        mode_data_re_up3_64['p_no_phase'] ,
#        markers[2%10]         , 
#        label = '64'       )
#
#ax1.plot(
#        mode_data_re_up3_128['Rad'],
#        mode_data_re_up3_128['p_no_phase']  , 
#        markers[3%10],
#        label = '128')
#
#ax1.plot(
#        mode_data_re_up3_256['Rad'],
#        mode_data_re_up3_256['p_no_phase']  , 
#        markers[4%10],
#        label = '256'       )
#        
#ax2.plot( 
#        mode_data_im_up3_32['Rad'],
#        mode_data_im_up3_32['p_no_phase'] ,
#        markers[1%10]       ,
#        mode_data_im_up3_64['Rad'],
#        mode_data_im_up3_64['p_no_phase'] ,
#        markers[2%10]       ,
#        mode_data_im_up3_128['Rad'],
#        mode_data_im_up3_128['p_no_phase']  , 
#        markers[3%10],
#        mode_data_im_up3_256['Rad'],
#        mode_data_im_up3_256['p_no_phase']  , 
#        markers[4%10]          )
#        
#ax1.set_ylabel('[Real]')
#ax2.set_ylabel('[Imaginary]')
#ax2.set_xlabel('Radius')
#ax1.legend(loc="upper right") 
#plt.savefig('tex-outputs/egv_m3.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
#
#
## gamma -3
#fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))
#
#ax1.plot( 
#        mode_data_re_up4_32['Rad'],
#        mode_data_re_up4_32['p_no_phase'] , 
#        markers[1%10],
#        label = '32')
#
#ax1.plot(
#        mode_data_re_up4_64['Rad'],
#        mode_data_re_up4_64['p_no_phase'] ,
#        markers[2%10]         , 
#        label = '64'       )
#
#ax1.plot(
#        mode_data_re_up4_128['Rad'],
#        mode_data_re_up4_128['p_no_phase']  , 
#        markers[3%10],
#        label = '128')
#
#ax1.plot(
#        mode_data_re_up4_256['Rad'],
#        mode_data_re_up4_256['p_no_phase']  , 
#        markers[4%10],
#        label = '256'       )
#        
#ax2.plot( 
#        mode_data_im_up4_32['Rad'],
#        mode_data_im_up4_32['p_no_phase'] ,
#        markers[1%10]       ,
#        mode_data_im_up4_64['Rad'],
#        mode_data_im_up4_64['p_no_phase'] ,
#        markers[2%10]       ,
#        mode_data_im_up4_128['Rad'],
#        mode_data_im_up4_128['p_no_phase']  , 
#        markers[3%10],
#        mode_data_im_up4_256['Rad'],
#        mode_data_im_up4_256['p_no_phase']  , 
#        markers[4%10]          )
#        
#ax1.set_ylabel('[Real]')
#ax2.set_ylabel('[Imaginary]')
#ax2.set_xlabel('Radius')
#ax1.legend(loc="upper right") 
#plt.savefig('tex-outputs/egv_m4.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
#
#
## gamma -4
#fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))
#
#ax1.plot( 
#        mode_data_re_up5_32['Rad'],
#        mode_data_re_up5_32['p_no_phase'] , 
#        markers[1%10],
#        label = '32')
#
#ax1.plot(
#        mode_data_re_up5_64['Rad'],
#        mode_data_re_up5_64['p_no_phase'] ,
#        markers[2%10]         , 
#        label = '64'       )
#
#ax1.plot(
#        mode_data_re_up5_128['Rad'],
#        mode_data_re_up5_128['p_no_phase']  , 
#        markers[3%10],
#        label = '128')
#
#ax1.plot(
#        mode_data_re_up5_256['Rad'],
#        mode_data_re_up5_256['p_no_phase']  , 
#        markers[4%10],
#        label = '256'       )
#        
#ax2.plot( 
#        mode_data_im_up5_32['Rad'],
#        mode_data_im_up5_32['p_no_phase'] ,
#        markers[1%10]       ,
#        mode_data_im_up5_64['Rad'],
#        mode_data_im_up5_64['p_no_phase'] ,
#        markers[2%10]       ,
#        mode_data_im_up5_128['Rad'],
#        mode_data_im_up5_128['p_no_phase']  , 
#        markers[3%10],
#        mode_data_im_up5_256['Rad'],
#        mode_data_im_up5_256['p_no_phase']  , 
#        markers[4%10]          )
#        
#ax1.set_ylabel('[Real]')
#ax2.set_ylabel('[Imaginary]')
#ax2.set_xlabel('Radius')
#ax1.legend(loc="upper right") 
#plt.savefig('tex-outputs/egv_m5.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
#
#
## gamma -5
#fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))
#
#ax1.plot( 
#        mode_data_re_up6_32['Rad'],
#        mode_data_re_up6_32['p_no_phase'] , 
#        markers[1%10],
#        label = '32')
#
#ax1.plot(
#        mode_data_re_up6_64['Rad'],
#        mode_data_re_up6_64['p_no_phase'] ,
#        markers[2%10]         , 
#        label = '64'       )
#
#ax1.plot(
#        mode_data_re_up6_128['Rad'],
#        mode_data_re_up6_128['p_no_phase']  , 
#        markers[3%10],
#        label = '128')
#
#ax1.plot(
#        mode_data_re_up6_256['Rad'],
#        mode_data_re_up6_256['p_no_phase']  , 
#        markers[4%10],
#        label = '256'       )
#        
#ax2.plot( 
#        mode_data_im_up6_32['Rad'],
#        mode_data_im_up6_32['p_no_phase'] ,
#        markers[1%10]       ,
#        mode_data_im_up6_64['Rad'],
#        mode_data_im_up6_64['p_no_phase'] ,
#        markers[2%10]       ,
#        mode_data_im_up6_128['Rad'],
#        mode_data_im_up6_128['p_no_phase']  , 
#        markers[3%10],
#        mode_data_im_up6_256['Rad'],
#        mode_data_im_up6_256['p_no_phase']  , 
#        markers[4%10]          )
#        
#ax1.set_ylabel('[Real]')
#ax2.set_ylabel('[Imaginary]')
#ax2.set_xlabel('Radius')
#ax1.legend(loc="upper right") 
#plt.savefig('tex-outputs/egv_m6.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
#
#
## gamma -6
#fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))
#
#ax1.plot( 
#        mode_data_re_up7_32['Rad'],
#        mode_data_re_up7_32['p_no_phase'] , 
#        markers[1%10],
#        label = '32')
#
#ax1.plot(
#        mode_data_re_up7_64['Rad'],
#        mode_data_re_up7_64['p_no_phase'] ,
#        markers[2%10]         , 
#        label = '64'       )
#
#ax1.plot(
#        mode_data_re_up7_128['Rad'],
#        mode_data_re_up7_128['p_no_phase']  , 
#        markers[3%10],
#        label = '128')
#
#ax1.plot(
#        mode_data_re_up7_256['Rad'],
#        mode_data_re_up7_256['p_no_phase']  , 
#        markers[4%10],
#        label = '256'       )
#        
#ax2.plot( 
#        mode_data_im_up7_32['Rad'],
#        mode_data_im_up7_32['p_no_phase'] ,
#        markers[1%10]       ,
#        mode_data_im_up7_64['Rad'],
#        mode_data_im_up7_64['p_no_phase'] ,
#        markers[2%10]       ,
#        mode_data_im_up7_128['Rad'],
#        mode_data_im_up7_128['p_no_phase']  , 
#        markers[3%10],
#        mode_data_im_up7_256['Rad'],
#        mode_data_im_up7_256['p_no_phase']  , 
#        markers[4%10]          )
#        
#ax1.set_ylabel('[Real]')
#ax2.set_ylabel('[Imaginary]')
#ax2.set_xlabel('Radius')
#ax1.legend(loc="upper right") 
#plt.savefig('tex-outputs/egv_m7.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
#
#
## gamma -7
#fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))
##
##ax1.plot( 
##        mode_data_re_up8_32['Rad'],
##        mode_data_re_up8_32['p_no_phase'] , 
##        markers[1%10],
##        label = '32')
##
#ax1.plot(
#        mode_data_re_up8_64['Rad'],
#        mode_data_re_up8_64['p_no_phase'] ,
#        markers[2%10]         , 
#        label = '64'       )
#
#ax1.plot(
#        mode_data_re_up8_128['Rad'],
#        mode_data_re_up8_128['p_no_phase']  , 
#        markers[3%10],
#        label = '128')
#
#ax1.plot(
#        mode_data_re_up8_256['Rad'],
#        mode_data_re_up8_256['p_no_phase']  , 
#        markers[4%10],
#        label = '256'       )
#        
#ax2.plot( 
#        mode_data_im_up8_64['Rad'],
#        mode_data_im_up8_64['p_no_phase'] ,
#        markers[2%10]       ,
#        mode_data_im_up8_128['Rad'],
#        mode_data_im_up8_128['p_no_phase']  , 
#        markers[3%10],
#        mode_data_im_up8_256['Rad'],
#        mode_data_im_up8_256['p_no_phase']  , 
#        markers[4%10]          )
#        
#ax1.set_ylabel('[Real]')
#ax2.set_ylabel('[Imaginary]')
#ax2.set_xlabel('Radius')
#ax1.legend(loc="upper right") 
#plt.savefig('tex-outputs/egv_m8.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
#
#
## gamma -8
#fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))
##
##ax1.plot( 
##        mode_data_re_up9_32['Rad'],
##        mode_data_re_up9_32['p_no_phase'] , 
##        markers[1%10],
##        label = '32')
##
#ax1.plot(
#        mode_data_re_up9_64['Rad'],
#        mode_data_re_up9_64['p_no_phase'] ,
#        markers[2%10]         , 
#        label = '64'       )
#
#ax1.plot(
#        mode_data_re_up9_128['Rad'],
#        mode_data_re_up9_128['p_no_phase']  , 
#        markers[3%10],
#        label = '128')
#
#ax1.plot(
#        mode_data_re_up9_256['Rad'],
#        mode_data_re_up9_256['p_no_phase']  , 
#        markers[4%10],
#        label = '256'       )
#        
#ax2.plot( 
#       # mode_data_im_up9_32['Rad'],
#        # mode_data_im_up9_32['p_no_phase'] ,
#        # markers[1%10]       ,
#        mode_data_im_up9_64['Rad'],
#        mode_data_im_up9_64['p_no_phase'] ,
#        markers[2%10]       ,
#        mode_data_im_up9_128['Rad'],
#        mode_data_im_up9_128['p_no_phase']  , 
#        markers[3%10],
#        mode_data_im_up9_256['Rad'],
#        mode_data_im_up9_256['p_no_phase']  , 
#        markers[4%10]          )
#        
#ax1.set_ylabel('[Real]')
#ax2.set_ylabel('[Imaginary]')
#ax2.set_xlabel('Radius')
#ax1.legend(loc="upper right") 
#plt.savefig('tex-outputs/egv_m9.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
#
#fig, ax = plt.subplots(
#        nrows=1,
#        ncols=1,
#        sharex=True,
#        figsize=set_size(width),
#        )
#
#plt.scatter(
#        gam_non_acc_data64['Re{gam/ak}'],
#        gam_non_acc_data64['Im{gam/ak}'],
#        marker = '.',
#        s = 1)
#plt.savefig('tex-outputs/gam.nonconv.scatter64.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
#fig, ax = plt.subplots(
#        nrows=1,
#        ncols=1,
#        sharex=True,
#        figsize=set_size(width),
#        )
#plt.scatter(
#        gam_non_acc_data128['Re{gam/ak}'],
#        gam_non_acc_data128['Im{gam/ak}'],
#        marker = '.',
#        s = 1)
#plt.savefig('tex-outputs/gam.nonconv.scatter128.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
#fig, ax = plt.subplots(
#        nrows=1,
#        ncols=1,
#        sharex=True,
#        figsize=set_size(width),
#        )
#plt.scatter(
#        gam_non_acc_data256['Re{gam/ak}'],
#        gam_non_acc_data256['Im{gam/ak}'],
#        marker = '.',
#        s = 1)
#plt.savefig('tex-outputs/gam.nonconv.scatter256.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
#
