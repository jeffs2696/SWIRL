#!/usr/bin/env python
# coding: utf-8

# In[1]:

import re
import sys
import tikzplotlib
import glob
import os
import pandas as pd
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

def savefig(*args, **kwargs):
    plt.savefig(*args, **kwargs) 
    plt.close(plt.gcf())

def extract_number(f):
    s = re.findall("\d+$",f)
    return (int(s[0]) if s else -1,f)

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
directory_32  ='../04-EVanalysis/SWIRLVerification/Table4_3/SecondOrderDiff/32pts/'
directory_64  ='../04-EVanalysis/SWIRLVerification/Table4_3/SecondOrderDiff/64pts/'
directory_128 ='../04-EVanalysis/SWIRLVerification/Table4_3/SecondOrderDiff/128pts/'
directory_256 ='../04-EVanalysis/SWIRLVerification/Table4_3/SecondOrderDiff/256pts/'
zero_crossings = ['0']
propagation_index_32 = ['0034', '0033', '0031', '0029', '0027', '0025', '0023', 
                        '0064', '0062', '0060', '0058', '0056', '0054', '0052']

propagation_index_64 = ['0068', '0067', '0065', '0063', '0061', '0059', '0055', '0053', '0051',
                        '0128', '0126', '0124', '0122', '0120', '0118', '0116', '0114', '0113']

propagation_index_128 = ['0234', '0233', '0231', '0229', '0227', '0223', '0213', '0119','0117' ,
                         '0256', '0254', '0252', '0250', '0248', '0246', '0244', '0242','0240' ]

propagation_index_256 = ['0494', '0493', '0491', '0489', '0487', '0485', '0481', '0477', '0473','0451',
                         '0512', '0510', '0508', '0506', '0504', '0502', '0500', '0498', '0496', '0467']
egvfile_32 = []
egvfile_64 = []
egvfile_128 = []
egvfile_256 = []
for i in range(len(propagation_index_32)):
    egvfile_32.append(directory_32 + 'egv.'+  propagation_index_32[i])
    
for i in range(len(propagation_index_64)):
    egvfile_64.append(directory_64 + 'egv.'+  propagation_index_64[i])
    
for i in range(len(propagation_index_128)):
    egvfile_128.append(directory_128 + 'egv.'+  propagation_index_128[i])

for i in range(len(propagation_index_256)):
    egvfile_256.append(directory_256 + 'egv.'+  propagation_index_256[i])

mode_data_32  = []
mode_data_64  = []
mode_data_128  = []
mode_data_256  = []
for i in range(len(propagation_index_32)): 
    mode_data_32.append(
        fcn.importPlotData(
            str(egvfile_32 [i])
        )
    )
for i in range(len(propagation_index_64)): 
    mode_data_64.append(
        fcn.importPlotData(
            str(egvfile_64 [i])
        )
    )
for i in range(len(propagation_index_128)): 
    mode_data_128.append(
        fcn.importPlotData(
            str(egvfile_128 [i])
        )
    )
for i in range(len(propagation_index_256)): 
    mode_data_256.append(
        fcn.importPlotData(
            str(egvfile_256 [i])
        )
    )


# modes with 32 points
# downstream
#mode_data_re1_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvre.034')
#mode_data_im1_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvim.034')
#
#mode_data_re2_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvre.033')
#mode_data_im2_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvim.033')
#
#mode_data_re3_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvre.031')
#mode_data_im3_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvim.031')
#
#mode_data_re4_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvre.029')
#mode_data_im4_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvim.029')
#
#mode_data_re5_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvre.027')
#mode_data_im5_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvim.027')
#
#mode_data_re6_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvre.025')
#mode_data_im6_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvim.025')
#
#mode_data_re7_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvre.023')
#mode_data_im7_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvim.023')
#
## upstream
# 
#mode_data_re_up1_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvre.064')
#mode_data_im_up1_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvim.064')
#
#mode_data_re_up2_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvre.062')
#mode_data_im_up2_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvim.062')
#
#mode_data_re_up3_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvre.060')
#mode_data_im_up3_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvim.060')
#
#mode_data_re_up4_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvre.058')
#mode_data_im_up4_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvim.058')
#
#mode_data_re_up5_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvre.056')
#mode_data_im_up5_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvim.056')
#
#mode_data_re_up6_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvre.054')
#mode_data_im_up6_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvim.054')
#
#mode_data_re_up7_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvre.052')
#mode_data_im_up7_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvim.052')
#
#
## modes with 64 points
## downstream
#
#mode_data_re1_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.068')
#mode_data_im1_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.068')
#
#mode_data_re2_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.067')
#mode_data_im2_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.067')
#
#mode_data_re3_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.065')
#mode_data_im3_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.065')
#
#mode_data_re4_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.063')
#mode_data_im4_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.063')
#
#mode_data_re5_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.061')
#mode_data_im5_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.061')
#
#mode_data_re6_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.059')
#mode_data_im6_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.059')
#
#mode_data_re7_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.055')
#mode_data_im7_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.055')
#
#mode_data_re8_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.053')
#mode_data_im8_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.053')
#
#mode_data_re9_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.051')
#mode_data_im9_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.051')
#
## upstream
#
#mode_data_re_up1_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.128')
#mode_data_im_up1_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.128')
#
#mode_data_re_up2_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.126')
#mode_data_im_up2_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.126')
#
#mode_data_re_up3_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.124')
#mode_data_im_up3_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.124')
#
#mode_data_re_up4_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.122')
#mode_data_im_up4_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.122')
#
#mode_data_re_up5_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.120')
#mode_data_im_up5_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.120')
#
#mode_data_re_up6_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.118')
#mode_data_im_up6_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.118')
#
#mode_data_re_up7_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.116')
#mode_data_im_up7_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.116')
#
#mode_data_re_up8_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.114')
#mode_data_im_up8_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.114')
#
#mode_data_re_up9_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.113')
#mode_data_im_up9_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.113')
##
##mode_data_re10_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.128')
##mode_data_im10_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.128')
##
#
#mode_data_re1_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvre.234')
#mode_data_im1_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvim.234')
#                                                                                               
#mode_data_re2_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvre.233')
#mode_data_im2_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvim.233')
#                                                                                               
#mode_data_re3_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvre.231')
#mode_data_im3_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvim.231')
#                                                                                               
#mode_data_re4_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvre.229')
#mode_data_im4_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvim.229')
#                                                                                               
#mode_data_re5_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvre.227')
#mode_data_im5_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvim.227')
#                                                                                               
#mode_data_re6_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvre.223')
#mode_data_im6_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvim.223')
#                                                                                               
#mode_data_re7_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvre.213')
#mode_data_im7_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvim.213')
#                                                                                               
#mode_data_re8_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvre.119')
#mode_data_im8_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvim.119')
#                                                                                               
#mode_data_re9_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvre.117')
#mode_data_im9_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvim.117')
#
#
#mode_data_re_up1_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvre.256')
#mode_data_im_up1_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvim.256')
#                                                                                                  
#mode_data_re_up2_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvre.254')
#mode_data_im_up2_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvim.254')
#                                                                                                  
#mode_data_re_up3_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvre.252')
#mode_data_im_up3_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvim.252')
#                                                                                                  
#mode_data_re_up4_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvre.250')
#mode_data_im_up4_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvim.250')
#                                                                                                  
#mode_data_re_up5_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvre.248')
#mode_data_im_up5_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvim.248')
#                                                                                                  
#mode_data_re_up6_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvre.246')
#mode_data_im_up6_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvim.246')
#                                                                                                
#mode_data_re_up7_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvre.244')
#mode_data_im_up7_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvim.244')
#                                                                                                  
#mode_data_re_up8_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvre.242')
#mode_data_im_up8_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvim.242')
#                                                                                                  
#mode_data_re_up9_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvre.240')
#mode_data_im_up9_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvim.240')
#
#
#mode_data_re1_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvre.494')
#mode_data_im1_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvim.494')
#
#mode_data_re2_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvre.493')
#mode_data_im2_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvim.493')
#
#mode_data_re3_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvre.491')
#mode_data_im3_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvim.491')
#
#mode_data_re4_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvre.489')
#mode_data_im4_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvim.489')
#
#mode_data_re5_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvre.487')
#mode_data_im5_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvim.487')
#
#mode_data_re6_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvre.485')
#mode_data_im6_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvim.485')
#
#mode_data_re7_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvre.481')
#mode_data_im7_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvim.481')
#
#mode_data_re8_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvre.477')
#mode_data_im8_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvim.477')
#
#mode_data_re9_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvre.473')
#mode_data_im9_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvim.473')
#
#mode_data_re_up1_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvre.512')
#mode_data_im_up1_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvim.512')
#
#mode_data_re_up2_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvre.510')
#mode_data_im_up2_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvim.510')
#
#mode_data_re_up3_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvre.508')
#mode_data_im_up3_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvim.508')
#
#mode_data_re_up4_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvre.506')
#mode_data_im_up4_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvim.506')
#
#mode_data_re_up5_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvre.504')
#mode_data_im_up5_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvim.504')
#
#mode_data_re_up6_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvre.502')
#mode_data_im_up6_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvim.502')
#
#mode_data_re_up7_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvre.500')
#mode_data_im_up7_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvim.500')
#
#mode_data_re_up8_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvre.498')
#mode_data_im_up8_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvim.498')
#
#mode_data_re_up9_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvre.496')
#mode_data_im_up9_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvim.496')
#
#gam_non_acc_data32  = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/gam.nonconv.0032')
#gam_non_acc_data64  = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/gam.nonconv.0064')
#gam_non_acc_data128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/gam.nonconv.0128')
#gam_non_acc_data256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/gam.nonconv.0256')
#
#gam_non_acc_data32_1 = gam_non_acc_data32.truncate(after=45)
#gam_non_acc_data32_2 = gam_non_acc_data32.truncate(before=45,after=90)
#
#gam_non_acc_data64 = gam_non_acc_data64.truncate(after=45)
#gam_non_acc_data128 = gam_non_acc_data128.truncate(after=45)
#gam_non_acc_data256 = gam_non_acc_data256.truncate(after=45)
#
#with open('../03-plotReport/tex-outputs/gam.non.acc.table4_3_32pts1.tex','w') as tex_file:
#    contents = gam_non_acc_data32_1.to_latex(index=False)
#    tex_file.write(contents)
#with open(("../03-plotReport/tex-outputs/gam.non.acc.table4_3_32pts1.tex"), "r+") as f:
#    text = f.read().replace("\\begin{tabular}", "\small \\begin{tabular}")
#    f.seek(0)
#    f.write(text)
#    f.close()
#
#with open('../03-plotReport/tex-outputs/gam.non.acc.table4_3_32pts2.tex','w') as tex_file:
#    contents = gam_non_acc_data32_2.to_latex(index=False)
#    tex_file.write(contents)
#with open(("../03-plotReport/tex-outputs/gam.non.acc.table4_3_32pts2.tex"), "r+") as f:
#    text = f.read().replace("\\begin{tabular}", "\small \\begin{tabular}")
#    f.seek(0)
#    f.write(text)
#    f.close()
#
#with open('../03-plotReport/tex-outputs/gam.non.acc.table4_3_64pts.tex','w') as tex_file:
#    contents = gam_non_acc_data64.to_latex(index=False)
#    tex_file.write(contents)
#with open(("../03-plotReport/tex-outputs/gam.non.acc.table4_3_64pts.tex"), "r+") as f:
#    text = f.read().replace("\\begin{tabular}", "\small \\begin{tabular}")
#    f.seek(0)
#    f.write(text)
#    f.close()
#with open('../03-plotReport/tex-outputs/gam.non.acc.table4_3_128pts.tex','w') as tex_file:
#    contents = gam_non_acc_data128.to_latex(index=False)
#    tex_file.write(contents)
#with open(("../03-plotReport/tex-outputs/gam.non.acc.table4_3_128pts.tex"), "r+") as f:
#    text = f.read().replace("\\begin{tabular}", "\small \\begin{tabular}")
#    f.seek(0)
#    f.write(text)
#    f.close()
#with open('../03-plotReport/tex-outputs/gam.non.acc.table4_3_256pts.tex','w') as tex_file:
#    contents = gam_non_acc_data256.to_latex(index=False)
#    tex_file.write(contents)
#with open(("../03-plotReport/tex-outputs/gam.non.acc.table4_3_256pts.tex"), "r+") as f:
#    text = f.read().replace("\\begin{tabular}", "\small \\begin{tabular}")
#    f.seek(0)
#    f.write(text)
#    f.close()
#
num_of_modes = 2
y_str = ['p_no_phase[Re]', 'p_no_phase[Im]']
markers = ['->', '-+', '-', '-.', '--', '.', 'x', 'X', 'D', '|']
# fig = plt.figure(constrained_layout=True, figsize=(10, 8))
# s = fig.add_gridspec(3, 3, width_ratios = [2, 3, 4], height_ratios = [3, 3, 2])
#for row in range(3):
#    for col in range(3):
#        ax = fig.add_subplot(s[row, col])
#
# fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))
#fig, ax = plt.subplots(
#        len(y_str), num_of_modes,
#        figsize=set_size(width), 
#        sharex=True , 
#        sharey=True , 
#        )
plt.figure(figsize=(15,12))
for m in range(num_of_modes):
    # for i in range(len(y_str)):
        # gamma +0
        ax1 = plt.subplot(len(y_str), num_of_modes, m+1)
        ax2 = plt.subplot(len(y_str), num_of_modes, m+3)
        ax1.plot( 
                mode_data_32[m]['Rad'],
                mode_data_32[m][y_str[0]] , 
                markers[1%10],
                label = '32')

        ax1.plot( 
                mode_data_64[m]['Rad'],
                mode_data_64[m][y_str[0]] , 
                markers[2%10],
                label = '64',)
        ax1.plot( 
                mode_data_128[m]['Rad'],
                mode_data_128[m][y_str[0]] , 
                markers[3%10],
                label = '128')
        ax1.plot( 
                mode_data_256[m]['Rad'],
                mode_data_256[m][y_str[0]], 
                markers[4%10],
                label = '256')
        ax2.plot( 
                mode_data_32[m]['Rad'],
                mode_data_32[m][y_str[1]] , 
                markers[1%10],
                label = '32')

        ax2.plot( 
                mode_data_64[m]['Rad'],
                mode_data_64[m][y_str[1]] , 
                markers[2%10],
                label = '64',)
        ax2.plot( 
                mode_data_128[m]['Rad'],
                mode_data_128[m][y_str[1]] , 
                markers[3%10],
                label = '128')
        ax2.plot( 
                mode_data_256[m]['Rad'],
                mode_data_256[m][y_str[1]], 
                markers[4%10],
                label = '256')
plt.show()
sys.exit()
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
# plt.show()
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
#plt.savefig('tex-outputs/gam.nonconv.scatter32.pdf', 
#        format = 'pdf', 
#        bbox_inches='tight')
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
