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

# print(mpl.rcParams.keys())
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

#list_of_files = glob.glob('../01-mean-flow/mean-flow????.dat')
# list_of_files2 = glob.glob('../04-EVanalysis/gammas????.Table4.3')
# list_of_files3 = glob.glob('../04-EVanalysis/gam????.acc.Table4.3')

#last_file = (max(list_of_files,key=extract_number))
# last_file2 = (max(list_of_files2,key=extract_number))
# last_file3 = (max(list_of_files3,key=extract_number))

#max_grid = int(re.findall('\d+$',last_file)[0])


#max_grid = (max(list_of_files,key=extract_number))
#max_grid = (max(list_of_files2,key=extract_number))
#max_grid =int(re.findall('\d+$',last_file)[0]) 

#print(round(int(max_grid)/10))
#print(max_grid)

# importing data
#flow_data    = fcn.importPlotData(last_file)
# gammas_data = fcn.importPlotData(last_file2)
# gam_nonconv_data_32 = fcn.importPlotData(last_file3)

# modes with 32 points
# downstream
 
mode_data_re1_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvre.034')
mode_data_im1_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvim.034')

mode_data_re2_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvre.033')
mode_data_im2_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvim.033')

mode_data_re3_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvre.031')
mode_data_im3_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvim.031')

mode_data_re4_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvre.029')
mode_data_im4_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvim.029')

mode_data_re5_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvre.027')
mode_data_im5_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvim.027')

mode_data_re6_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvre.025')
mode_data_im6_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvim.025')

mode_data_re7_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvre.023')
mode_data_im7_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvim.023')

# upstream
 
mode_data_re_up1_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvre.064')
mode_data_im_up1_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvim.064')

mode_data_re_up2_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvre.062')
mode_data_im_up2_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvim.062')

mode_data_re_up3_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvre.060')
mode_data_im_up3_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvim.060')

mode_data_re_up4_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvre.058')
mode_data_im_up4_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvim.058')

mode_data_re_up5_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvre.056')
mode_data_im_up5_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvim.056')

mode_data_re_up6_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvre.054')
mode_data_im_up6_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvim.054')

mode_data_re_up7_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvre.052')
mode_data_im_up7_32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/egvim.052')


# modes with 64 points
# downstream

mode_data_re1_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.068')
mode_data_im1_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.068')

mode_data_re2_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.067')
mode_data_im2_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.067')

mode_data_re3_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.065')
mode_data_im3_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.065')

mode_data_re4_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.063')
mode_data_im4_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.063')

mode_data_re5_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.061')
mode_data_im5_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.061')

mode_data_re6_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.059')
mode_data_im6_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.059')

mode_data_re7_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.055')
mode_data_im7_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.055')

mode_data_re8_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.053')
mode_data_im8_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.053')

mode_data_re9_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.051')
mode_data_im9_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.051')
#
#mode_data_re10_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.128')
#mode_data_im10_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.128')
#

# upstream

mode_data_re_up1_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.068')
mode_data_im_up1_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.068')

mode_data_re_up2_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.067')
mode_data_im_up2_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.067')

mode_data_re_up3_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.065')
mode_data_im_up3_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.065')

mode_data_re_up4_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.063')
mode_data_im_up4_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.063')

mode_data_re_up5_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.061')
mode_data_im_up5_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.061')

mode_data_re_up6_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.059')
mode_data_im_up6_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.059')

mode_data_re_up7_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.055')
mode_data_im_up7_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.055')

mode_data_re_up8_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.053')
mode_data_im_up8_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.053')

mode_data_re_up9_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.051')
mode_data_im_up9_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.051')
#
#mode_data_re10_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvre.128')
#mode_data_im10_64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/egvim.128')
#

mode_data_re1_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvre.234')
mode_data_im1_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvim.234')

mode_data_re2_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvre.233')
mode_data_im2_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvim.233')

mode_data_re3_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvre.231')
mode_data_im3_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvim.231')

mode_data_re4_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvre.229')
mode_data_im4_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvim.229')

mode_data_re5_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvre.227')
mode_data_im5_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvim.227')

mode_data_re6_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvre.223')
mode_data_im6_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvim.223')

mode_data_re7_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvre.213')
mode_data_im7_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvim.213')

mode_data_re8_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvre.119')
mode_data_im8_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvim.119')

mode_data_re9_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvre.117')
mode_data_im9_128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/egvim.117')


mode_data_re1_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvre.494')
mode_data_im1_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvim.494')

mode_data_re2_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvre.493')
mode_data_im2_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvim.493')

mode_data_re3_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvre.491')
mode_data_im3_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvim.491')

mode_data_re4_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvre.487')
mode_data_im4_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvim.487')

mode_data_re5_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvre.485')
mode_data_im5_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvim.485')

mode_data_re6_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvre.481')
mode_data_im6_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvim.481')

mode_data_re7_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvre.477')
mode_data_im7_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvim.477')

mode_data_re8_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvre.473')
mode_data_im8_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvim.473')

mode_data_re9_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvre.469')
mode_data_im9_256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/egvim.469')

# mode_data_re4 = fcn.importPlotData('../04-EVanalysis/egvre.064')
# mode_data_im4 = fcn.importPlotData('../04-EVanalysis/egvim.064')
# cv_wave_data = fcn.importPlotData('../04-EVanalysis/cv.waves.dat')
gam_non_acc_data32 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/32pts/gam.nonconv.0032')
gam_non_acc_data64 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/64pts/gam.nonconv.0064')
gam_non_acc_data128 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/128pts/gam.nonconv.0128')
gam_non_acc_data256 = fcn.importPlotData('../04-EVanalysis/SWIRLVerification/Table4_3/256pts/gam.nonconv.0256')

# print(gam_non_acc_data32)
# LEE_L2_data  = fcn.importPlotData('../02-method-of-manufactured-solutions/L2-LEE.dat')
# LEE_ROC_data = fcn.importPlotData('../02-method-of-manufactured-solutions/ROC-LEE.dat')
# SND_L2_data  = fcn.importPlotData('../02-method-of-manufactured-solutions/L2-sound_speed-.dat')
# SND_ROC_data = fcn.importPlotData('../02-method-of-manufactured-solutions/ROC-sound_speed.dat')

# Delta_r    = LEE_ROC_data.Delta_r
# LEE_ROC    = LEE_ROC_data.ROC
# SND_ROC    = SND_ROC_data.ROC
# LEE_L2    = LEE_L2_data.L2
# SND_L2    = SND_L2_data.L2
# GridPoints = LEE_L2_data.GridPoints

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

#with open('../03-plotReport/tex-outputs/cv.waves.table.tex','w') as tex_file:
#    contents = cv_wave_data.to_latex(index=True)
#    tex_file.write(contents)
##
##with open('../03-plotReport/tex-outputs/gam.acc.table.tex','w') as tex_file:
##    contents = gam_acc_data.to_latex(index=True)
##    tex_file.write(contents)
##
gam_non_acc_data32_1 = gam_non_acc_data32.truncate(after=45)
gam_non_acc_data32_2 = gam_non_acc_data32.truncate(before=45,after=90)

gam_non_acc_data64 = gam_non_acc_data64.truncate(after=45)
gam_non_acc_data128 = gam_non_acc_data128.truncate(after=45)
gam_non_acc_data256 = gam_non_acc_data256.truncate(after=45)

with open('../03-plotReport/tex-outputs/gam.non.acc.table4_3_32pts1.tex','w') as tex_file:
    contents = gam_non_acc_data32_1.to_latex(index=False)
    tex_file.write(contents)
with open(("../03-plotReport/tex-outputs/gam.non.acc.table4_3_32pts1.tex"), "r+") as f:
    text = f.read().replace("\\begin{tabular}", "\small \\begin{tabular}")
    f.seek(0)
    f.write(text)
    f.close()

with open('../03-plotReport/tex-outputs/gam.non.acc.table4_3_32pts2.tex','w') as tex_file:
    contents = gam_non_acc_data32_2.to_latex(index=False)
    tex_file.write(contents)
with open(("../03-plotReport/tex-outputs/gam.non.acc.table4_3_32pts2.tex"), "r+") as f:
    text = f.read().replace("\\begin{tabular}", "\small \\begin{tabular}")
    f.seek(0)
    f.write(text)
    f.close()

with open('../03-plotReport/tex-outputs/gam.non.acc.table4_3_64pts.tex','w') as tex_file:
    contents = gam_non_acc_data64.to_latex(index=False)
    tex_file.write(contents)
with open(("../03-plotReport/tex-outputs/gam.non.acc.table4_3_64pts.tex"), "r+") as f:
    text = f.read().replace("\\begin{tabular}", "\small \\begin{tabular}")
    f.seek(0)
    f.write(text)
    f.close()
with open('../03-plotReport/tex-outputs/gam.non.acc.table4_3_128pts.tex','w') as tex_file:
    contents = gam_non_acc_data128.to_latex(index=False)
    tex_file.write(contents)
with open(("../03-plotReport/tex-outputs/gam.non.acc.table4_3_128pts.tex"), "r+") as f:
    text = f.read().replace("\\begin{tabular}", "\small \\begin{tabular}")
    f.seek(0)
    f.write(text)
    f.close()

with open('../03-plotReport/tex-outputs/gam.non.acc.table4_3_256pts.tex','w') as tex_file:
    contents = gam_non_acc_data256.to_latex(index=False)
    tex_file.write(contents)

with open(("../03-plotReport/tex-outputs/gam.non.acc.table4_3_256pts.tex"), "r+") as f:
    text = f.read().replace("\\begin{tabular}", "\small \\begin{tabular}")
    f.seek(0)
    f.write(text)
    f.close()

#print(tabulate(cv_wave_data.iloc[0:],headers =cv_wave_data.columns,tablefmt='latex'))



# In[3]:
markers = ['->', '-+', '-', '-.', '--', '.', 'x', 'X', 'D', '|']


# gamma +0
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))

ax1.plot( 
        mode_data_re1_32['Rad'],
        mode_data_re1_32['p_no_phase'] , 
        markers[1%10],
        label = '32')

ax1.plot(
        mode_data_re1_64['Rad'],
        mode_data_re1_64['p_no_phase'] ,
        markers[2%10]         , 
        label = '64'       )

ax1.plot(
        mode_data_re1_128['Rad'],
        mode_data_re1_128['p_no_phase']  , 
        markers[3%10],
        label = '128')

ax1.plot(
        mode_data_re1_256['Rad'],
        mode_data_re1_256['p_no_phase']  , 
        markers[4%10],
        label = '256'       )
        
ax2.plot( 
        mode_data_im1_32['Rad'],
        mode_data_im1_32['p_no_phase'] ,
        markers[1%10]       ,
        mode_data_im1_64['Rad'],
        mode_data_im1_64['p_no_phase'] ,
        markers[2%10]       ,
        mode_data_im1_128['Rad'],
        mode_data_im1_128['p_no_phase']  , 
        markers[3%10],
        mode_data_im1_256['Rad'],
        mode_data_im1_256['p_no_phase']  , 
        markers[4%10]          )
        
ax1.set_ylabel('[Real]')
ax2.set_ylabel('[Imaginary]')
ax2.set_xlabel('Radius')
ax1.legend(loc="upper right") 
plt.savefig('tex-outputs/egv1.pdf', 
        format = 'pdf', 
        bbox_inches='tight')

# gamma +1
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))

ax1.plot( 
        mode_data_re2_32['Rad'],
        mode_data_re2_32['p_no_phase'] , 
        markers[1%10],
        label = '32')

ax1.plot( 
        mode_data_re2_64['Rad'],
        mode_data_re2_64['p_no_phase'] ,
        markers[2%10]         , 
        label = '64'       )

ax1.plot(
        mode_data_re2_128['Rad'],
        mode_data_re2_128['p_no_phase']  , 
        markers[3%10],
        label = '128')

ax1.plot( 
        mode_data_re2_256['Rad'],
        mode_data_re2_256['p_no_phase']  , 
        markers[4%10],
        label = '256'       )
        
ax2.plot( 
        mode_data_im2_32['Rad'],
        mode_data_im2_32['p_no_phase'] ,
        markers[1%10]       ,
        # label = '32'        ,
        mode_data_im2_64['Rad'],
        mode_data_im2_64['p_no_phase'] ,
        markers[2%10]       ,
        #label = '64'        ,
        mode_data_im2_128['Rad'],
        mode_data_im2_128['p_no_phase']  , 
        markers[3%10],
        #label = '128'        ,
        mode_data_im2_256['Rad'],
        mode_data_im2_256['p_no_phase']  , 
        markers[4%10]          )

ax1.set_ylabel('[Real]')
ax2.set_ylabel('[Imaginary]')
ax2.set_xlabel('Radius')
ax1.legend()
        
plt.savefig('tex-outputs/egv2.pdf', 
        format = 'pdf', 
        bbox_inches='tight')

# gamma +2
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))

ax1.plot( 
        mode_data_re3_32['Rad'],
        mode_data_re3_32['p_no_phase'] , 
        markers[1%10],
        label = '32')

ax1.plot( 
        mode_data_re3_64['Rad'],
        mode_data_re3_64['p_no_phase'] ,
        markers[2%10]         , 
        label = '64'       )

ax1.plot(
        mode_data_re3_128['Rad'],
        mode_data_re3_128['p_no_phase']  , 
        markers[3%10],
        label = '128')

ax1.plot( 
        mode_data_re3_256['Rad'],
        mode_data_re3_256['p_no_phase'] , 
        markers[4%10],
        label = '256')

ax2.plot( 
        mode_data_im3_32['Rad'],
        mode_data_im3_32['p_no_phase'] ,
        # label = '32'        ,
        markers[1%10]       ,
        mode_data_im3_64['Rad'],
        mode_data_im3_64['p_no_phase'] ,
        # label = '64'        ,
        markers[2%10]       ,
        mode_data_im3_128['Rad'],
        mode_data_im3_128['p_no_phase']  , 
        # label = '128'        ,
        markers[3%10],
        mode_data_im3_256['Rad'],
        mode_data_im3_256['p_no_phase']  , 
        # label = '256'        ,
        markers[4%10])
        

ax1.set_ylabel('[Real]')
ax2.set_ylabel('[Imaginary]')
ax2.set_xlabel('Radius')
ax1.legend(loc="upper left") 

plt.savefig(
        'tex-outputs/egv3.pdf', 
        format = 'pdf', 
        bbox_inches='tight')


# gamma +3
fig, (ax1, ax2) = plt.subplots(
        2, 1, figsize=set_size(width))

ax1.plot( 
        mode_data_re4_32['Rad'],
        mode_data_re4_32['p_no_phase'] , 
        markers[1%10],
        label = '32')
ax1.plot(
        mode_data_re4_64['Rad'],
        mode_data_re4_64['p_no_phase'] ,
        markers[2%10]         , 
        label = '64'       )
ax1.plot(
        mode_data_re4_128['Rad'],
        mode_data_re4_128['p_no_phase']  , 
        markers[3%10],
        label = '128')
ax1.plot(
        mode_data_re4_256['Rad'],
        mode_data_re4_256['p_no_phase']  , 
        markers[4%10],
        label = '256'       )
        
ax2.plot( 
        mode_data_im4_32['Rad'],
        mode_data_im4_32['p_no_phase'] ,
        markers[1%10]       ,
        # label = '32'        ,
        mode_data_im4_64['Rad'],
        mode_data_im4_64['p_no_phase'] ,
        markers[2%10]       ,
        #label = '64'        ,
        mode_data_im4_128['Rad'],
        mode_data_im4_128['p_no_phase']  , 
        markers[3%10],
        #label = '128'        ,
        mode_data_im4_256['Rad'],
        mode_data_im4_256['p_no_phase']  , 
        markers[4%10]                  )
        
ax1.set_ylabel('[Real]')
ax2.set_ylabel('[Imaginary]')
ax2.set_xlabel('Radius')
ax1.legend(loc="upper right") 
plt.savefig('tex-outputs/egv4.pdf', 
        format = 'pdf', 
        bbox_inches='tight')
#----

# gamma +4
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))


ax1.plot( 
        mode_data_re5_32['Rad'],
        mode_data_re5_32['p_no_phase'] , 
        markers[1%10],
        label = '32')

ax1.plot(
        mode_data_re5_64['Rad'],
        mode_data_re5_64['p_no_phase'] ,
        markers[2%10]         , 
        label = '64'       )
ax1.plot(
        mode_data_re5_128['Rad'],
        mode_data_re5_128['p_no_phase']  , 
        markers[3%10],
        label = '128')
ax1.plot(
        mode_data_re5_256['Rad'],
        mode_data_re5_256['p_no_phase']  , 
        markers[4%10],
        label = '256'       )
ax2.plot( 
        mode_data_im5_32['Rad'],
        mode_data_im5_32['p_no_phase'] ,
        markers[1%10]       ,
        mode_data_im5_64['Rad'],
        mode_data_im5_64['p_no_phase'] ,
        markers[2%10]       ,
        mode_data_im5_128['Rad'],
        mode_data_im5_128['p_no_phase']  , 
        markers[3%10],
        mode_data_im5_256['Rad'],
        mode_data_im5_256['p_no_phase']  , 
        markers[4%10]          )

ax1.set_ylabel('[Real]')
ax2.set_ylabel('[Imaginary]')
ax2.set_xlabel('Radius')
ax1.legend(loc="upper right") 
plt.savefig('tex-outputs/egv5.pdf', 
        format = 'pdf', 
        bbox_inches='tight')

# gamma +5
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))
ax1.plot( 
        mode_data_re6_32['Rad'],
        mode_data_re6_32['p_no_phase'] , 
        markers[1%10],
        label = '32')

ax1.plot(
        mode_data_re6_64['Rad'],
        mode_data_re6_64['p_no_phase'] ,
        markers[2%10]         , 
        label = '64'       )
ax1.plot(
        mode_data_re6_128['Rad'],
        mode_data_re6_128['p_no_phase']  , 
        markers[3%10],
        label = '128')
ax1.plot(
        mode_data_re6_256['Rad'],
        mode_data_re6_256['p_no_phase']  , 
        markers[4%10],
        label = '256'       )
ax2.plot( 
        mode_data_im6_32['Rad'],
        mode_data_im6_32['p_no_phase'] ,
        markers[1%10]       ,
        mode_data_im6_64['Rad'],
        mode_data_im6_64['p_no_phase'] ,
        markers[2%10]       ,
        mode_data_im6_128['Rad'],
        mode_data_im6_128['p_no_phase']  , 
        markers[3%10],
        mode_data_im6_256['Rad'],
        mode_data_im6_256['p_no_phase']  , 
        markers[4%10]         ) 
ax1.set_ylabel('[Real]')
ax2.set_ylabel('[Imaginary]')
ax2.set_xlabel('Radius')
ax1.legend(loc="upper right") 
plt.savefig('tex-outputs/egv6.pdf', 
        format = 'pdf', 
        bbox_inches='tight')

# gamma +6
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))
ax1.plot( 
        mode_data_re7_32['Rad'],
        mode_data_re7_32['p_no_phase'] , 
        markers[1%10],
        label = '32')

ax1.plot(
        mode_data_re7_64['Rad'],
        mode_data_re7_64['p_no_phase'] ,
        markers[2%10]         , 
        label = '64'       )
ax1.plot(
        mode_data_re7_128['Rad'],
        mode_data_re7_128['p_no_phase']  , 
        markers[3%10],
        label = '128')
ax1.plot(
        mode_data_re7_256['Rad'],
        mode_data_re7_256['p_no_phase']  , 
        markers[4%10],
        label = '256'       )
ax2.plot( 
        mode_data_im7_32['Rad'],
        mode_data_im7_32['p_no_phase'] ,
        markers[1%10]       ,
        mode_data_im7_64['Rad'],
        mode_data_im7_64['p_no_phase'] ,
        markers[2%10]       ,
        mode_data_im7_128['Rad'],
        mode_data_im7_128['p_no_phase']  , 
        markers[3%10],
        mode_data_im7_256['Rad'],
        mode_data_im7_256['p_no_phase']  , 
        markers[4%10]         )
ax1.set_ylabel('[Real]')
ax2.set_ylabel('[Imaginary]')
ax2.set_xlabel('Radius')
ax1.legend(loc="upper right") 
plt.savefig('tex-outputs/egv7.pdf', 
        format = 'pdf', 
        bbox_inches='tight')

# gamma +7
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))
ax1.plot(
        mode_data_re8_64['Rad'],
        mode_data_re8_64['p_no_phase'] ,
        markers[2%10]         , 
        label = '64'       )
ax1.plot(
        mode_data_re8_128['Rad'],
        mode_data_re8_128['p_no_phase']  , 
        markers[3%10],
        label = '128')
ax1.plot(
        mode_data_re8_256['Rad'],
        mode_data_re8_256['p_no_phase']  , 
        markers[4%10],
        label = '256'       )
ax2.plot( 
        mode_data_im8_64['Rad'],
        mode_data_im8_64['p_no_phase'] ,
        markers[2%10]       ,
        mode_data_im8_128['Rad'],
        mode_data_im8_128['p_no_phase']  , 
        markers[3%10],
        mode_data_im8_256['Rad'],
        mode_data_im8_256['p_no_phase']  , 
        markers[4%10]          )
ax1.set_ylabel('[Real]')
ax2.set_ylabel('[Imaginary]')
ax2.set_xlabel('Radius')
ax1.legend(loc="upper right") 
plt.savefig('tex-outputs/egv8.pdf', 
        format = 'pdf', 
        bbox_inches='tight')

# gamma +9
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))
ax1.plot(
        mode_data_re9_64['Rad'],
        mode_data_re9_64['p_no_phase'] ,
        markers[2%10]         , 
        label = '64'       )
ax1.plot(
        mode_data_re9_128['Rad'],
        mode_data_re9_128['p_no_phase']  , 
        markers[3%10],
        label = '128')
ax1.plot(
        mode_data_re9_256['Rad'],
        mode_data_re9_256['p_no_phase']  , 
        markers[4%10],
        label = '256'       )

ax2.plot( 
        mode_data_im9_64['Rad'],
        mode_data_im9_64['p_no_phase'] ,
        markers[2%10]       ,
        mode_data_im9_128['Rad'],
        mode_data_im9_128['p_no_phase']  , 
        markers[3%10],
        mode_data_im9_256['Rad'],
        mode_data_im9_256['p_no_phase']  , 
        markers[4%10]          )

ax1.set_ylabel('[Real]')
ax2.set_ylabel('[Imaginary]')
ax2.set_xlabel('Radius')
ax1.legend(loc="upper right") 
plt.savefig('tex-outputs/egv9.pdf', 
        format = 'pdf', 
        bbox_inches='tight')


# gamma +0
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))

ax1.plot( 
        mode_data_re1_32['Rad'],
        mode_data_re1_32['p_no_phase'] , 
        markers[1%10],
        label = '32')

ax1.plot(
        mode_data_re1_64['Rad'],
        mode_data_re1_64['p_mg'] ,
        markers[2%10]         , 
        label = '64'       )

ax1.plot(
        mode_data_re1_128['Rad'],
        mode_data_re1_128['p_mg']  , 
        markers[3%10],
        label = '128')

ax1.plot(
        mode_data_re1_256['Rad'],
        mode_data_re1_256['p_mg']  , 
        markers[4%10],
        label = '256'       )
        
ax2.plot( 
        mode_data_im1_32['Rad'],
        mode_data_im1_32['p_mg'] ,
        markers[1%10]       ,
        mode_data_im1_64['Rad'],
        mode_data_im1_64['p_mg'] ,
        markers[2%10]       ,
        mode_data_im1_128['Rad'],
        mode_data_im1_128['p_mg']  , 
        markers[3%10],
        mode_data_im1_256['Rad'],
        mode_data_im1_256['p_mg']  , 
        markers[4%10]          )
        
ax1.set_ylabel('[Real]')
ax2.set_ylabel('[Imaginary]')
ax2.set_xlabel('Radius')
ax1.legend(loc="upper right") 
plt.savefig('tex-outputs/egvMag1.pdf', 
        format = 'pdf', 
        bbox_inches='tight')

# gamma +1
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))

ax1.plot( 
        mode_data_re2_32['Rad'],
        mode_data_re2_32['p_mg'] , 
        markers[1%10],
        label = '32')

ax1.plot( 
        mode_data_re2_64['Rad'],
        mode_data_re2_64['p_mg'] ,
        markers[2%10]         , 
        label = '64'       )

ax1.plot(
        mode_data_re2_128['Rad'],
        mode_data_re2_128['p_mg']  , 
        markers[3%10],
        label = '128')

ax1.plot( 
        mode_data_re2_256['Rad'],
        mode_data_re2_256['p_mg']  , 
        markers[4%10],
        label = '256'       )
        
ax2.plot( 
        mode_data_im2_32['Rad'],
        mode_data_im2_32['p_mg'] ,
        markers[1%10]       ,
        # label = '32'        ,
        mode_data_im2_64['Rad'],
        mode_data_im2_64['p_mg'] ,
        markers[2%10]       ,
        #label = '64'        ,
        mode_data_im2_128['Rad'],
        mode_data_im2_128['p_mg']  , 
        markers[3%10],
        #label = '128'        ,
        mode_data_im2_256['Rad'],
        mode_data_im2_256['p_mg']  , 
        markers[4%10]          )

ax1.set_ylabel('[Real]')
ax2.set_ylabel('[Imaginary]')
ax2.set_xlabel('Radius')
ax1.legend()
        
plt.savefig('tex-outputs/egvMag2.pdf', 
        format = 'pdf', 
        bbox_inches='tight')

# gamma +2
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))

ax1.plot( 
        mode_data_re3_32['Rad'],
        mode_data_re3_32['p_mg'] , 
        markers[1%10],
        label = '32')

ax1.plot( 
        mode_data_re3_64['Rad'],
        mode_data_re3_64['p_mg'] ,
        markers[2%10]         , 
        label = '64'       )

ax1.plot(
        mode_data_re3_128['Rad'],
        mode_data_re3_128['p_mg']  , 
        markers[3%10],
        label = '128')

ax1.plot( 
        mode_data_re3_256['Rad'],
        mode_data_re3_256['p_mg'] , 
        markers[4%10],
        label = '256')

ax2.plot( 
        mode_data_im3_32['Rad'],
        mode_data_im3_32['p_mg'] ,
        # label = '32'        ,
        markers[1%10]       ,
        mode_data_im3_64['Rad'],
        mode_data_im3_64['p_mg'] ,
        # label = '64'        ,
        markers[2%10]       ,
        mode_data_im3_128['Rad'],
        mode_data_im3_128['p_mg']  , 
        # label = '128'        ,
        markers[3%10],
        mode_data_im3_256['Rad'],
        mode_data_im3_256['p_mg']  , 
        # label = '256'        ,
        markers[4%10])
        

ax1.set_ylabel('[Real]')
ax2.set_ylabel('[Imaginary]')
ax2.set_xlabel('Radius')
ax1.legend(loc="upper left") 

plt.savefig(
        'tex-outputs/egvMag3.pdf', 
        format = 'pdf', 
        bbox_inches='tight')


# gamma +3
fig, (ax1, ax2) = plt.subplots(
        2, 1, figsize=set_size(width))

ax1.plot( 
        mode_data_re4_32['Rad'],
        mode_data_re4_32['p_mg'] , 
        markers[1%10],
        label = '32')
ax1.plot(
        mode_data_re4_64['Rad'],
        mode_data_re4_64['p_mg'] ,
        markers[2%10]         , 
        label = '64'       )
ax1.plot(
        mode_data_re4_128['Rad'],
        mode_data_re4_128['p_mg']  , 
        markers[3%10],
        label = '128')
ax1.plot(
        mode_data_re4_256['Rad'],
        mode_data_re4_256['p_mg']  , 
        markers[4%10],
        label = '256'       )
        
ax2.plot( 
        mode_data_im4_32['Rad'],
        mode_data_im4_32['p_mg'] ,
        markers[1%10]       ,
        # label = '32'        ,
        mode_data_im4_64['Rad'],
        mode_data_im4_64['p_mg'] ,
        markers[2%10]       ,
        #label = '64'        ,
        mode_data_im4_128['Rad'],
        mode_data_im4_128['p_mg']  , 
        markers[3%10],
        #label = '128'        ,
        mode_data_im4_256['Rad'],
        mode_data_im4_256['p_mg']  , 
        markers[4%10]                  )
        
ax1.set_ylabel('[Real]')
ax2.set_ylabel('[Imaginary]')
ax2.set_xlabel('Radius')
ax1.legend(loc="upper right") 
plt.savefig('tex-outputs/egvMag4.pdf', 
        format = 'pdf', 
        bbox_inches='tight')
#----

# gamma +4
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))


ax1.plot( 
        mode_data_re5_32['Rad'],
        mode_data_re5_32['p_mg'] , 
        markers[1%10],
        label = '32')

ax1.plot(
        mode_data_re5_64['Rad'],
        mode_data_re5_64['p_mg'] ,
        markers[2%10]         , 
        label = '64'       )
ax1.plot(
        mode_data_re5_128['Rad'],
        mode_data_re5_128['p_mg']  , 
        markers[3%10],
        label = '128')
ax1.plot(
        mode_data_re5_256['Rad'],
        mode_data_re5_256['p_mg']  , 
        markers[4%10],
        label = '256'       )
ax2.plot( 
        mode_data_im5_32['Rad'],
        mode_data_im5_32['p_mg'] ,
        markers[1%10]       ,
        mode_data_im5_64['Rad'],
        mode_data_im5_64['p_mg'] ,
        markers[2%10]       ,
        mode_data_im5_128['Rad'],
        mode_data_im5_128['p_mg']  , 
        markers[3%10],
        mode_data_im5_256['Rad'],
        mode_data_im5_256['p_mg']  , 
        markers[4%10]          )

ax1.set_ylabel('[Real]')
ax2.set_ylabel('[Imaginary]')
ax2.set_xlabel('Radius')
ax1.legend(loc="upper right") 
plt.savefig('tex-outputs/egvMag5.pdf', 
        format = 'pdf', 
        bbox_inches='tight')

# gamma +5
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))
ax1.plot( 
        mode_data_re6_32['Rad'],
        mode_data_re6_32['p_mg'] , 
        markers[1%10],
        label = '32')

ax1.plot(
        mode_data_re6_64['Rad'],
        mode_data_re6_64['p_mg'] ,
        markers[2%10]         , 
        label = '64'       )
ax1.plot(
        mode_data_re6_128['Rad'],
        mode_data_re6_128['p_mg']  , 
        markers[3%10],
        label = '128')
ax1.plot(
        mode_data_re6_256['Rad'],
        mode_data_re6_256['p_mg']  , 
        markers[4%10],
        label = '256'       )
ax2.plot( 
        mode_data_im6_32['Rad'],
        mode_data_im6_32['p_mg'] ,
        markers[1%10]       ,
        mode_data_im6_64['Rad'],
        mode_data_im6_64['p_mg'] ,
        markers[2%10]       ,
        mode_data_im6_128['Rad'],
        mode_data_im6_128['p_mg']  , 
        markers[3%10],
        mode_data_im6_256['Rad'],
        mode_data_im6_256['p_mg']  , 
        markers[4%10]         ) 
ax1.set_ylabel('[Real]')
ax2.set_ylabel('[Imaginary]')
ax2.set_xlabel('Radius')
ax1.legend(loc="upper right") 
plt.savefig('tex-outputs/egvMag6.pdf', 
        format = 'pdf', 
        bbox_inches='tight')

# gamma +6
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))
ax1.plot( 
        mode_data_re7_32['Rad'],
        mode_data_re7_32['p_mg'] , 
        markers[1%10],
        label = '32')

ax1.plot(
        mode_data_re7_64['Rad'],
        mode_data_re7_64['p_mg'] ,
        markers[2%10]         , 
        label = '64'       )
ax1.plot(
        mode_data_re7_128['Rad'],
        mode_data_re7_128['p_mg']  , 
        markers[3%10],
        label = '128')
ax1.plot(
        mode_data_re7_256['Rad'],
        mode_data_re7_256['p_mg']  , 
        markers[4%10],
        label = '256'       )
ax2.plot( 
        mode_data_im7_32['Rad'],
        mode_data_im7_32['p_mg'] ,
        markers[1%10]       ,
        mode_data_im7_64['Rad'],
        mode_data_im7_64['p_mg'] ,
        markers[2%10]       ,
        mode_data_im7_128['Rad'],
        mode_data_im7_128['p_mg']  , 
        markers[3%10],
        mode_data_im7_256['Rad'],
        mode_data_im7_256['p_mg']  , 
        markers[4%10]         )
ax1.set_ylabel('[Real]')
ax2.set_ylabel('[Imaginary]')
ax2.set_xlabel('Radius')
ax1.legend(loc="upper right") 
plt.savefig('tex-outputs/egvMag7.pdf', 
        format = 'pdf', 
        bbox_inches='tight')

# gamma +7
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))
ax1.plot(
        mode_data_re8_64['Rad'],
        mode_data_re8_64['p_mg'] ,
        markers[2%10]         , 
        label = '64'       )
ax1.plot(
        mode_data_re8_128['Rad'],
        mode_data_re8_128['p_mg']  , 
        markers[3%10],
        label = '128')
ax1.plot(
        mode_data_re8_256['Rad'],
        mode_data_re8_256['p_mg']  , 
        markers[4%10],
        label = '256'       )
ax2.plot( 
        mode_data_im8_64['Rad'],
        mode_data_im8_64['p_mg'] ,
        markers[2%10]       ,
        mode_data_im8_128['Rad'],
        mode_data_im8_128['p_mg']  , 
        markers[3%10],
        mode_data_im8_256['Rad'],
        mode_data_im8_256['p_mg']  , 
        markers[4%10]          )
ax1.set_ylabel('[Real]')
ax2.set_ylabel('[Imaginary]')
ax2.set_xlabel('Radius')
ax1.legend(loc="upper right") 
plt.savefig('tex-outputs/egvMag8.pdf', 
        format = 'pdf', 
        bbox_inches='tight')

# gamma +9
fig, (ax1, ax2) = plt.subplots(2, 1, figsize=set_size(width))
ax1.plot(
        mode_data_re9_64['Rad'],
        mode_data_re9_64['p_mg'] ,
        markers[2%10]         , 
        label = '64'       )
ax1.plot(
        mode_data_re9_128['Rad'],
        mode_data_re9_128['p_mg']  , 
        markers[3%10],
        label = '128')
ax1.plot(
        mode_data_re9_256['Rad'],
        mode_data_re9_256['p_mg']  , 
        markers[4%10],
        label = '256'       )

ax2.plot( 
        mode_data_im9_64['Rad'],
        mode_data_im9_64['p_mg'] ,
        markers[2%10]       ,
        mode_data_im9_128['Rad'],
        mode_data_im9_128['p_mg']  , 
        markers[3%10],
        mode_data_im9_256['Rad'],
        mode_data_im9_256['p_mg']  , 
        markers[4%10]          )

ax1.set_ylabel('[Real]')
ax2.set_ylabel('[Imaginary]')
ax2.set_xlabel('Radius')
ax1.legend(loc="upper right") 
plt.savefig('tex-outputs/egvMag9.pdf', 
        format = 'pdf', 
        bbox_inches='tight')

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
