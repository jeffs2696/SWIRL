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
test_dir = [
        '../01-mean-flow/']


directories = [
    '../01-mean-flow/KousenT1/',
    '../01-mean-flow/KousenT2/',
    '../01-mean-flow/KousenT3/',
    '../01-mean-flow/KousenT4/',
    '../03-EVanalysis/SWIRLVerification/Table4_3/SecondOrderDiff/32pts/',
    '../03-EVanalysis/SWIRLVerification/Table4_3/SecondOrderDiff/64pts/',
    '../03-EVanalysis/SWIRLVerification/Table4_3/SecondOrderDiff/128pts/',
    '../03-EVanalysis/SWIRLVerification/Table4_3/SecondOrderDiff/256pts/',
    '../03-EVanalysis/SWIRLVerification/Table4_3/FourthOrderDiff/32pts/',
    '../03-EVanalysis/SWIRLVerification/Table4_3/FourthOrderDiff/64pts/',
    '../03-EVanalysis/SWIRLVerification/Table4_3/FourthOrderDiff/128pts/',
    '../03-EVanalysis/SWIRLVerification/Table4_3/FourthOrderDiff/256pts/',
    '../03-EVanalysis/SWIRLVerification/Table4_3/FourthOrderDiff/1025pts/',
    '../03-EVanalysis/SWIRLVerification/Table4_4/FourthOrderDiff/1025pts/',
    '../03-EVanalysis/SWIRLVerification/Table4_5/FourthOrderDiff/1025pts/',
    '../03-EVanalysis/SWIRLVerification/Table4_6/FourthOrderDiff/1025pts/']
# create a grid convergence study object based on a representative grid size


# Files to plot from SWIRL

# [I] Manufactured Solutions
# 1. Mean Flow Profile

MeanFlowData1_1 = fcn.importPlotData(
    directories[0] + 'mean-flow0032.dat')
MeanFlowData1_2 = fcn.importPlotData(
    directories[0] + 'mean-flow0032.dat')


MeanFlowData2_1 = fcn.importPlotData(
    directories[0] + 'mean-flow0064.dat')
MeanFlowData2_2 = fcn.importPlotData(
    directories[0] + 'mean-flow0064.dat')

MeanFlowData3_1 = fcn.importPlotData(
    directories[0] + 'mean-flow0128.dat')
MeanFlowData3_2 = fcn.importPlotData(
    directories[0] + 'mean-flow0128.dat')

MeanFlowData3_1 = fcn.importPlotData(
    directories[0] + 'mean-flow0256.dat')
MeanFlowData3_2 = fcn.importPlotData(
    directories[0] + 'mean-flow0256.dat')

MeanFlowData_T43 = fcn.importPlotData(
        directories[0] + 'mean-flow0128.dat')
AxialWavenumberData_T44 = fcn.importPlotData(
        directories[-4] + 'gam.nonconv_acc.1025')

MeanFlowData_T44 = fcn.importPlotData(
        directories[1] + 'mean-flow1025.dat')
AxialWavenumberData_T44 = fcn.importPlotData(
        directories[-3] + 'gam.nonconv_acc.1025')

MeanFlowData_T45 = fcn.importPlotData(
        directories[2] + 'mean-flow1025.dat')
AxialWavenumberData_T45 = fcn.importPlotData(
        directories[-2] + 'gam.nonconv_acc.1025')

MeanFlowData_T46 = fcn.importPlotData(
        directories[3] + 'mean-flow1025.dat')
AxialWavenumberData_T46 = fcn.importPlotData(
        directories[-1] + 'gam.nonconv_acc.1025')

fig, ax = plt.subplots(
    nrows=1,
    ncols=1,
    sharex=True,
    figsize=set_size(width),
)

fig.suptitle('Kousen T4.3 Mean Flow Profile')
ax.plot(
    MeanFlowData_T43['radius'],
    MeanFlowData_T43['M_x'],
    label=(r'$M_x$ '),
)

ax.plot(
    MeanFlowData_T43['radius'],
    MeanFlowData_T43['M_theta'],
    label=r"$M_{\theta}$",
)

ax.set_xlabel(r'Dimensionless Radius $\bar{r}$')
ax.set_ylabel(r'$M$')
ax.legend()

fig.savefig('tex-outputs/KousenT1_mean_flow_profile.pdf',
            format='pdf')  # , bbox_inches='tight')

fig, ax = plt.subplots(
    nrows=1,
    ncols=1,
    sharex=True,
    figsize=set_size(width),
)

fig.suptitle('Kousen T4.4 Mean Flow Profile')
ax.plot(
    MeanFlowData_T44['radius'],
    MeanFlowData_T44['M_x'],
    label=(r'$M_x$ '),
)

ax.plot(
    MeanFlowData_T44['radius'],
    MeanFlowData_T44['M_theta'],
    label=r"$M_{\theta}$",
)

ax.set_xlabel(r'Dimensionless Radius $\bar{r}$')
ax.set_ylabel(r'$M$')
ax.legend()

fig.savefig('tex-outputs/KousenT2_mean_flow_profile.pdf',
            format='pdf',
            bbox_inches='tight')

fig, ax = plt.subplots(
    nrows=1,
    ncols=1,
    sharex=True,
    figsize=set_size(width),
)

fig.suptitle('Kousen T4.5 Mean Flow Profile')
ax.plot(
    MeanFlowData_T45['radius'],
    MeanFlowData_T45['M_x'],
    label=(r'$M_x$ '),
)

ax.plot(
    MeanFlowData_T45['radius'],
    MeanFlowData_T45['M_theta'],
    label=r"$M_{\theta}$",
)

ax.set_xlabel(r'Dimensionless Radius $\bar{r}$')
ax.set_ylabel(r'$M$')
ax.legend()


fig.savefig('tex-outputs/KousenT3_mean_flow_profile.pdf',
            format='pdf',
            bbox_inches='tight')
fig, ax = plt.subplots(
    nrows=1,
    ncols=1,
    sharex=True,
    figsize=set_size(width),
)


fig.suptitle('Kousen T4.6 Mean Flow Profile')
ax.plot(
    MeanFlowData_T46['radius'],
    MeanFlowData_T46['M_x'],
    label=(r'$M_x$ '),
)

ax.plot(
    MeanFlowData_T46['radius'],
    MeanFlowData_T46['M_theta'],
    label=r"$M_{\theta}$",
)

ax.set_xlabel(r'Dimensionless Radius $\bar{r}$')
ax.set_ylabel(r'$M$')
ax.legend()

fig.savefig('tex-outputs/KousenT4_mean_flow_profile.pdf',
            format='pdf',
            bbox_inches='tight')

fig = plt.figure(
    constrained_layout=False,
#    figsize=set_size(width)
)
# plt.axvline(x = k_x_cutoff,color = 'black', label = 'cut-off line',lw=1)
plt.scatter(AxialWavenumberData_T46['Re{gam}'],AxialWavenumberData_T46['Im{gam}'],marker='.')#label='Upstream',)


# plt.scatter(k_x_real_down,k_x_imag_down,label='Downstream',marker='.')
# plt.scatter(k_x_convection,0,marker='.',label='$$k_{x,cv}$$')
plt.legend()
plt.xlabel(r'\textit{Real}$(k_x)$')
plt.ylabel(r'\textit{Imaginary}$(k_x)$')

fig.savefig('tex-outputs/KousenT4_gam_nonconv_scatter_4thOrderApprox.pdf',
            format='pdf',
            bbox_inches='tight')

