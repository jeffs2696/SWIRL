#!/usr/bin/env python
# coding: utf-8

# In[1]:
from cycler import cycler
import time
import math
import re
import sys
# import tikzplotlib
import glob
import os
import pandas as pd
import numpy as np
import matplotlib as mpl
# mpl.use('pdf')
import matplotlib.ticker as mticker
import plotReportLib
from matplotlib import pyplot as plt
from scipy.optimize import curve_fit
from pyGCS import GCS
import matplotlib as mpl
from plotReportLib import myfunctions as fcn
from tabulate import tabulate
from texttable import Texttable
import latextable
from my_plot import set_size
start = time.time()


def tryint(s):
    """
    Return an int if possible, or `s` unchanged.
    """
    try:
        return int(s)
    except ValueError:
        return s


def powlaw(x, a, b):
    return a * np.power(x, b)


def linlaw(x, a, b):
    return a + x * b


def curve_fit_log(xdata, ydata):
    """Fit data to a power law with weights according to a log scale"""
    # Weights according to a log scale
    # Apply fscalex
    xdata_log = np.log10(xdata)
    # Apply fscaley
    ydata_log = np.log10(ydata)
    # Fit linear
    popt_log, pcov_log = curve_fit(linlaw, xdata_log, ydata_log)
    #print(popt_log, pcov_log)
    # Apply fscaley^-1 to fitted data
    ydatafit_log = np.power(10, linlaw(xdata_log, *popt_log))
    # There is no need to apply fscalex^-1 as original data is already
    # available
    return (popt_log, pcov_log, ydatafit_log)


def alphanum_key(s):
    """
    Turn a string into a list of string and number chunks.
    >>> alphanum_key("z23a")
    ["z", 23, "a"]
    """
    return [tryint(c) for c in re.split('([0-9]+)', s)]


def human_sort(l):
    """
    Sort a list in the way that humans expect.
    """
    l.sort(key=alphanum_key)


def savefig(*args, **kwargs):
    plt.savefig(*args, **kwargs)
    plt.close(plt.gcf())


def extract_number(f):
    s = re.findall("\\d+$", f)
    return (int(s[0]) if s else -1, f)


# plot parameters
# Create cycler object. Use any styling from above you please
monochrome = (
    cycler(
        'color',
        ['royalblue', 'orangered', 'goldenrod', 'black']
    ) +
    cycler('linestyle', ['-', '--', ':', '-.']
           )  # + cycler( 'marker', ['.','^',',', '.']) + cycler( 'markevery',[125,150,175,100])
)

markers = ['o-', '+-', '--', '-', 'o-', '.', 'x', 'X', 'D', '|']
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

# plt.rcParams['figure.constrained_layout.use'] = True
plt.rcParams.update(tex_fonts)
plt.rcParams['axes.grid'] = True
plt.rcParams['axes.prop_cycle'] = monochrome
plt.rcParams['axes.spines.top'] = False
plt.rcParams['axes.spines.right'] = False
plt.rcParams['axes.spines.bottom'] = False
plt.rcParams['axes.spines.left'] = False

# input
directories = [
    '../01-mean-flow/MMS1/',
    '../02-method-of-manufactured-solutions/MMS1/',
    '../04-EVanalysis/MMS1/']
# create a grid convergence study object based on a representative grid size


# Files to plot from SWIRL

# [I] Manufactured Solutions
# 1. Mean Flow Profile
M_int = 3

MeanFlowData1_1 = fcn.importPlotData(
    directories[0] + 'mean-flow0257_FDmethod1.dat')
MeanFlowData1_2 = fcn.importPlotData(
    directories[0] + 'mean-flow0257_FDmethod2.dat')


MeanFlowData2_1 = fcn.importPlotData(
    directories[0] + 'mean-flow0513_FDmethod1.dat')
MeanFlowData2_2 = fcn.importPlotData(
    directories[0] + 'mean-flow0513_FDmethod2.dat')

MeanFlowData3_1 = fcn.importPlotData(
    directories[0] + 'mean-flow1025_FDmethod1.dat')
MeanFlowData3_2 = fcn.importPlotData(
    directories[0] + 'mean-flow1025_FDmethod2.dat')

# 2. Speed of Sound Error
SpeedOfSoundData1 = fcn.importPlotData(
    directories[1] + 'sound-speed-error0257.dat')
SpeedOfSoundData2 = fcn.importPlotData(
    directories[1] + 'sound-speed-error0513.dat')
SpeedOfSoundData3 = fcn.importPlotData(
    directories[1] + 'sound-speed-error1025.dat')

# 3. Perturbation Variables are in the mean-flow files (already imported)

# [II] Source Terms  - Created symbolically with SciPy, computed with F90

# 4a. 1
SourceTermData1_1 = fcn.importPlotData(
    directories[1] + 'SourceTermData1_0257_FDmethod1.dat')
SourceTermData1_2 = fcn.importPlotData(
    directories[1] + 'SourceTermData1_0513_FDmethod1.dat')
SourceTermData1_3 = fcn.importPlotData(
    directories[1] + 'SourceTermData1_1025_FDmethod1.dat')

# 4b. 2
SourceTermData2_1 = fcn.importPlotData(
    directories[1] + 'SourceTermData2_0257_FDmethod1.dat')
SourceTermData2_2 = fcn.importPlotData(
    directories[1] + 'SourceTermData2_0513_FDmethod1.dat')
SourceTermData2_3 = fcn.importPlotData(
    directories[1] + 'SourceTermData2_1025_FDmethod1.dat')

# 4c. 3
SourceTermData3_1 = fcn.importPlotData(
    directories[1] + 'SourceTermData3_0257_FDmethod1.dat')
SourceTermData3_2 = fcn.importPlotData(
    directories[1] + 'SourceTermData3_0513_FDmethod1.dat')
SourceTermData3_3 = fcn.importPlotData(
    directories[1] + 'SourceTermData3_1025_FDmethod1.dat')

# 4d. 4
SourceTermData4_1 = fcn.importPlotData(
    directories[1] + 'SourceTermData4_0257_FDmethod1.dat')
SourceTermData4_2 = fcn.importPlotData(
    directories[1] + 'SourceTermData4_0513_FDmethod1.dat')
SourceTermData4_3 = fcn.importPlotData(
    directories[1] + 'SourceTermData4_1025_FDmethod1.dat')

# [III] Grid Convergence Study

# Rates of convergence for each source term
LEE_L2_data1 = fcn.importPlotData(
    directories[1] +
    'L2-LEE_numberOfIterations9_FDmethod1.dat')
LEE_L2_data2 = fcn.importPlotData(
    directories[1] +
    'L2-LEE_numberOfIterations9_FDmethod2.dat')
LEE_ROC_data1 = fcn.importPlotData(
    directories[1] +
    'ROC-LEE_numberOfIterations9_FDmethod1.dat')
LEE_ROC_data2 = fcn.importPlotData(
    directories[1] +
    'ROC-LEE_numberOfIterations9_FDmethod2.dat')
SND_L2_data = fcn.importPlotData(
    directories[1] +
    'L2-sound_speed_numberOfIterations9.dat')
SND_ROC_data = fcn.importPlotData(directories[1] + 'ROC-sound_speed.dat')


# Plot Data

# Fig. 1:
fig, ax = plt.subplots(
    nrows=1,
    ncols=1,
    sharex=True,
    figsize=set_size(width),
)

fig.suptitle('MMS Mean Flow Profile')
ax.plot(
    MeanFlowData3_1['radius'],
    MeanFlowData3_1['M_x'],
    label=(r'$M_x$ '),
)

ax.plot(
    MeanFlowData3_1['radius'],
    MeanFlowData3_1['M_theta'],
    label=r"$M_{\theta}$",
)

ax.plot(
    MeanFlowData3_1['radius'],
    (MeanFlowData3_1['M_x']**2 + MeanFlowData3_1['M_theta']**2)**0.5,
    label=r"$M_{total}$",
)

ax.set_xlabel(r'Dimensionless Radius $\bar{r}$')
ax.set_ylabel(r'$M$')
ax.legend()


fig.savefig('tex-outputs/MMS_mean_flow_profile.pdf',
            format='pdf')  # , bbox_inches='tight')

# Fig. 2:
fig, ax = plt.subplots(
    nrows=1,
    ncols=1,
    sharex=True,
    figsize=set_size(width),
)
# + str(len(MeanFlowData1_1['radius'] )) + 'points')
fig.suptitle('Manufactured Pertubation Functions')
ax.plot(
    MeanFlowData3_1['radius'],
    MeanFlowData3_1['vR'],
    label=(r'$\bar{v}_r$ '),
    markevery=25)
ax.set_xlabel('Radius [-]')
ax.set_ylabel('Perturbation Velocity [-]')
ax.legend()
fig.savefig('tex-outputs/MMS_perturbation_variables_vR.pdf',
            format='pdf')  # , bbox_inches='tight')

# Fig. 3:
fig, ax = plt.subplots(
    nrows=1,
    ncols=1,
    sharex=True,
    figsize=set_size(width),
)
# + str(len(MeanFlowData1_1['radius'] )) + 'points')
fig.suptitle('Manufactured Pertubation Functions')
ax.plot(
    MeanFlowData1_1['radius'],
    MeanFlowData1_1['vTh'],
    label=(r'$\bar{v}_{\theta}$ '),
    markevery=25)

ax.legend()
fig.savefig('tex-outputs/MMS_perturbation_variables_vTh.pdf',
            format='pdf')  # , bbox_inches='tight')


# Fig. 4:
fig, ax = plt.subplots(
    nrows=1,
    ncols=1,
    sharex=True,
    figsize=set_size(width),
)
# + str(len(MeanFlowData1_1['radius'] )) + 'points')
fig.suptitle('Manufactured Pertubation Functions')
ax.plot(
    MeanFlowData3_1['radius'],
    MeanFlowData3_1['vX'],
    label=(r'$\bar{v}_x$ '),
    markevery=25)

ax.set_xlabel('Radius [-]')
ax.set_ylabel('Perturbation Velocity [-]')
ax.legend()

fig.savefig('tex-outputs/MMS_perturbation_variables_vX.pdf',
            format='pdf')  # , bbox_inches='tight')

# Fig. 5:
fig, ax = plt.subplots(
    nrows=1,
    ncols=1,
    sharex=True,
    figsize=set_size(width),
)
# + str(len(MeanFlowData1_1['radius'] )) + 'points')
fig.suptitle('Manufactured Pertubation Functions')
ax.plot(
    MeanFlowData3_1['radius'],
    MeanFlowData3_1['Pr'],
    label=(r'$\bar{P}$ '),
    markevery=25)

ax.set_xlabel('Radius [-]')
ax.set_ylabel('Perturbation Velocity [-]')
ax.legend()

fig.savefig('tex-outputs/MMS_perturbation_variables_Pr.pdf',
            format='pdf')  # , bbox_inches='tight')

# Fig. 6: Speed Of Sound - MMS Grid Comparison
fig, ax = plt.subplots(
    nrows=1,
    ncols=1,
    sharex=True,
    figsize=set_size(width),
)
fig.suptitle('Speed Of Sound Computation at 7 grid points')
ax.plot(
    MeanFlowData1_1['radius'],
    MeanFlowData1_1['A_expected'],
    markevery=55,
    label='Expected')

ax.plot(
    MeanFlowData1_1['radius'],
    MeanFlowData1_1['A_actual'],
    marker='.',
    markevery=25,
    label='Actual')

ax.set_xlabel('Radius [-]')
ax.set_ylabel('Speed Of Sound [-]')
plt.legend()

plt.savefig('tex-outputs/SpeedOfSoundComparison1.pdf',
            format='pdf',
            bbox_inches='tight')


# Fig. 7: Speed Of Sound - MMS error
fig, ax = plt.subplots(
    nrows=1,
    ncols=1,
    sharex=True,
    figsize=set_size(width),
)
fig.suptitle('Speed Of Sound Error at Multiple Grids')
ax.plot(
    SpeedOfSoundData1['radius'],
    abs(SpeedOfSoundData1['SpeedofSoundError']),
    markevery=25,
    label=str(len(SpeedOfSoundData1['radius'])))

ax.plot(
    SpeedOfSoundData2['radius'],
    abs(SpeedOfSoundData2['SpeedofSoundError']),
    markevery=25,
    label=str(len(SpeedOfSoundData2['radius'])))


ax.plot(
    SpeedOfSoundData3['radius'],
    abs(SpeedOfSoundData3['SpeedofSoundError']),
    markevery=25,
    label=str(len(SpeedOfSoundData3['radius'])))

plt.legend()
ax.set_ylabel(r'$\epsilon$')
ax.set_xlabel('Radius')
plt.savefig('tex-outputs/SpeedOfSoundComparison2.pdf',
            format='pdf',
            bbox_inches='tight')


# Fig. 8: Speed of Sound - L2 Norm from Numerical Integration
fig, ax = plt.subplots(
    nrows=1,
    ncols=1,
    sharex=True,
    figsize=set_size(width),
)
fig.suptitle(
    r' Log-log plot of the $L2_{norm}$ from the Speed of Sound Integration')

ax.loglog(
    SND_L2_data['GridPoints'],
    SND_L2_data['L2'],
    label='Calculated L2 Norm')

x = SND_L2_data['GridPoints'].to_numpy()
y = SND_L2_data['L2'].to_numpy()

b = SND_L2_data['L2'].iloc[0]  # y intercept
k = 2  # expected slope
y_expected = 10**(-np.log10(x**k) + np.log10(x[0]**k * b))
plt.plot(
    x,
    y_expected,
    label='Second Order Convergence'
)

plt.legend()

ax.set_ylabel("$ ln (\\hat{\\epsilon})  $")
plt.savefig('tex-outputs/LEE_ROC.pdf',
            format='pdf',
            bbox_inches='tight')

ax.set_xlabel("$ ln (N)  $")
# plt.show()
# sys.exit()

plt.savefig('tex-outputs/SpeedOfSoundComparisonL2.pdf',
            format='pdf',
            bbox_inches='tight')

# Fig. 9: Speed Of Sound - Rate Of Convergence
fig, ax = plt.subplots(
    nrows=1,
    ncols=1,
    sharex=True,
    figsize=set_size(width),
)

ax.semilogx(
    SND_ROC_data['Delta_r'],
    SND_ROC_data['ROC'],
    label='Approximated with Second Order Central')

ax.semilogx(
    SND_ROC_data['Delta_r'],
    2.0*np.ones(len(LEE_ROC_data1['Delta_r'])),
    label='Second Order Convergence')

ax.set_ylabel(r' Rate of Convergence, $\alpha$')
ax.set_xlabel('Number of Grid Points')

plt.savefig('tex-outputs/SpeedOfSoundComparisonROC.pdf',
            format='pdf',
            bbox_inches='tight')

# Fig. 10: LEE - L2 Norm from manufactured solution
fig, ax = plt.subplots(
    nrows=1,
    ncols=1,
    sharex=True,
    figsize=set_size(width),
)

fig.suptitle('L2 of LEE Matrix')
ax.loglog(
    LEE_L2_data1['GridPoints'],
    LEE_L2_data1['L2'],
    label='Approximated with 2nd Order Central')

ax.loglog(
    LEE_L2_data2['GridPoints'],
    LEE_L2_data2['L2'],
    label='Approximated with 4th Order Central')


x = LEE_L2_data1['GridPoints'].to_numpy()
y = LEE_L2_data1['L2'].to_numpy()

b = LEE_L2_data1['L2'].iloc[0]  # y intercept
k = 2  # expected slope
y_expected = 10**(-np.log10(x**k) + np.log10(x[0]**k * b))
plt.plot(
    x,
    y_expected,
    label='Second Order Convergence'
)


x = LEE_L2_data2['GridPoints'].to_numpy()
y = LEE_L2_data2['L2'].to_numpy()

b = LEE_L2_data2['L2'].iloc[0]  # y intercept
k = 4  # expected slope
y_expected = 10**(-np.log10(x**k) + np.log10(x[0]**k * b))
plt.plot(
    x,
    y_expected,
    label='Fourth Order Convergence'
)

ax.legend()
ax.set_ylabel(r' Rate of Convergence, $\alpha$')
ax.set_xlabel('Number of Grid Points')
plt.savefig('tex-outputs/LEE_L2.pdf',
            format='pdf',
            bbox_inches='tight')

# Fig. 11: LEE - Rate Of Convergence 
fig, ax = plt.subplots(
    nrows=1,
    ncols=1,
    sharex=True,
    figsize=set_size(width),
)
fig.suptitle('Rate of Convergence of LEE Matrix')
ax.semilogx(
    LEE_ROC_data1['Delta_r'],
    LEE_ROC_data1['ROC'],
    label='Approximated with Second Order Central')

ax.semilogx(
    LEE_ROC_data1['Delta_r'],
    2.0*np.ones(len(LEE_ROC_data1['Delta_r'])),
    label='Second Order Convergence')


ax.semilogx(
    LEE_ROC_data1['Delta_r'],
    4.0*np.ones(len(LEE_ROC_data1['Delta_r'])),
    label='Fourth Order Convergence')

ax.semilogx(
    LEE_ROC_data2['Delta_r'],
    LEE_ROC_data2['ROC'],
    label='Approximated with Fourth Order Central')
ax.legend()
ax.set_xlabel('$$\\Delta r$$')
ax.set_ylabel('Rate Of Convergence')
plt.savefig('tex-outputs/LEE_ROC.pdf',
            format='pdf',
            bbox_inches='tight')


# Fig. 12: MMS LEE Source Term 1

fig, axs = plt.subplots(
    nrows=1,
    ncols=1,
    sharex=True,
    figsize=set_size(width),
)

fig.suptitle('LEE First Source Term Comparison')

axs.plot(
    SourceTermData1_1['radius'],
    SourceTermData1_1['S_actual'],
    label='Numerical Approximation')
axs.plot(
    SourceTermData1_1['radius'],
    SourceTermData1_1['S_expected'],
    label='$Theoretical$')
axs.set(ylabel='$S_1$')
axs.set(xlabel='$r [-]$')


axs.legend()
plt.savefig('tex-outputs/SourceTermComparison1.pdf',
            format='pdf',
            bbox_inches='tight')


fig, axs = plt.subplots(
    nrows=1,
    ncols=1,
    sharex=True,
    figsize=set_size(width),
)

fig.suptitle('LEE Second Source Term Comparison')

axs.plot(
    SourceTermData2_1['radius'],
    SourceTermData2_1['S_actual'],
    label='Numerical Approximation')
axs.plot(
    SourceTermData2_1['radius'],
    SourceTermData2_1['S_expected'],
    label='$Theoretical$')
axs.set(ylabel='$S_2$')
axs.set(xlabel='$r [-]$')


axs.legend()
plt.savefig('tex-outputs/SourceTermComparison2.pdf',
            format='pdf',
            bbox_inches='tight')


fig, axs = plt.subplots(
    nrows=1,
    ncols=1,
    sharex=True,
    figsize=set_size(width),
)

fig.suptitle('LEE Third Source Term Comparison')

axs.plot(
    SourceTermData3_1['radius'],
    SourceTermData3_1['S_actual'],
    label='Numerical Approximation')
axs.plot(
    SourceTermData3_1['radius'],
    SourceTermData3_1['S_expected'],
    label='$Theoretical$')
axs.set(ylabel='$S_3$')
axs.set(xlabel='$r [-]$')


axs.legend()
plt.savefig('tex-outputs/SourceTermComparison3.pdf',
            format='pdf',
            bbox_inches='tight')


fig, axs = plt.subplots(
    nrows=1,
    ncols=1,
    sharex=True,
    figsize=set_size(width),
)

fig.suptitle('LEE Fourth Source Term Comparison')

axs.plot(
    SourceTermData4_1['radius'],
    SourceTermData4_1['S_actual'],
    label='Numerical Approximation')
axs.plot(
    SourceTermData4_1['radius'],
    SourceTermData4_1['S_expected'],
    label='$Theoretical$')
axs.set(ylabel='$S_4$')
axs.set(xlabel='$r [-]$')


axs.legend()
plt.savefig('tex-outputs/SourceTermComparison4.pdf',
            format='pdf',
            bbox_inches='tight')

# Fig. 13:

fig, axs = plt.subplots(
    nrows=1,
    ncols=1,
    sharex=True,
    figsize=set_size(width),
)

fig.suptitle('Source Term 1 Error at Multiple Grids')

axs.semilogy(
    SourceTermData1_1['radius'],
    abs(SourceTermData1_1['S_actual'] - SourceTermData1_1['S_expected']))

axs.semilogy(
    SourceTermData1_2['radius'],
    abs(SourceTermData1_2['S_actual'] - SourceTermData1_2['S_expected']))

axs.semilogy(
    SourceTermData1_3['radius'],
    abs(SourceTermData1_3['S_actual'] - SourceTermData1_3['S_expected']))

axs.set_ylabel('$$\\epsilon$$')
axs.set_xlabel('$$ r$$')
plt.savefig('tex-outputs/SourceTermError1.pdf',
            format='pdf',
            bbox_inches='tight')

# Fig. 14:
fig, axs = plt.subplots(
    nrows=1,
    ncols=1,
    sharex=True,
    figsize=set_size(width),
)
axs.semilogy(
    SourceTermData2_1['radius'],
    abs(SourceTermData2_1['S_actual'] - SourceTermData2_1['S_expected']))

axs.semilogy(
    SourceTermData2_2['radius'],
    abs(SourceTermData2_2['S_actual'] - SourceTermData2_2['S_expected']))

axs.semilogy(
    SourceTermData2_3['radius'],
    abs(SourceTermData2_3['S_actual'] - SourceTermData2_3['S_expected']))


fig.suptitle('Source Term 2 Error at Multiple Grids')

axs.set_ylabel('$$\\epsilon$$')
axs.set_xlabel('$$ r$$')
plt.savefig('tex-outputs/SourceTermError2.pdf',
            format='pdf',
            bbox_inches='tight')


# Fig. 15:
fig, axs = plt.subplots(
    nrows=1,
    ncols=1,
    sharex=True,
    figsize=set_size(width),
)
axs.semilogy(
    SourceTermData3_1['radius'],
    abs(SourceTermData3_1['S_actual'] - SourceTermData3_1['S_expected']))

axs.semilogy(
    SourceTermData3_2['radius'],
    abs(SourceTermData3_2['S_actual'] - SourceTermData3_2['S_expected']))

axs.semilogy(
    SourceTermData3_3['radius'],
    abs(SourceTermData3_3['S_actual'] - SourceTermData3_3['S_expected']))


axs.set_ylabel('$$\\epsilon$$')
axs.set_xlabel('$$ r$$')
fig.suptitle('Source Term 3 Error at Multiple Grids')

axs.set_ylabel('$$\\epsilon$$')
axs.set_xlabel('$$ r$$')
plt.savefig('tex-outputs/SourceTermError3.pdf',
            format='pdf',
            bbox_inches='tight')


# Fig. 16:
fig, axs = plt.subplots(
    nrows=1,
    ncols=1,
    sharex=True,
    figsize=set_size(width),
)
axs.semilogy(
    SourceTermData4_1['radius'],
    abs(SourceTermData4_1['S_actual'] - SourceTermData4_1['S_expected']))

axs.semilogy(
    SourceTermData4_2['radius'],
    abs(SourceTermData4_2['S_actual'] - SourceTermData4_2['S_expected']))

axs.semilogy(
    SourceTermData4_3['radius'],
    abs(SourceTermData4_3['S_actual'] - SourceTermData4_3['S_expected']))

axs.set_ylabel('$$\\epsilon$$')
axs.set_xlabel('$$ r$$')
fig.suptitle('Source Term 4 Error at Multiple Grids')
plt.savefig('tex-outputs/SourceTermError4.pdf',
            format='pdf',
            bbox_inches='tight')

# plt.show()
# sys.exit()
