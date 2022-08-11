#!/usr/bin/env python
# coding: utf-8

# In[1]:

import sys
import math
from plotReportLib import myfunctions as fcn
import numpy as np
import scipy as scip
from scipy import special
import cmath
import matplotlib as mpl
import matplotlib.pyplot as plt
def set_size(width, fraction=1):
    """Set figure dimensions to avoid scaling in LaTeX.  
    Parameters
    ----------
    width: float
    Document textwidth or columnwidth in pts
    fraction: float, optional
    Fraction of the width which you wish the figure to occupy 

    Returns
    -------
    fig_dim: tuple
    Dimensions of figure in inches
    """
    # Width of figure (in pts)
    fig_width_pt = width * fraction 
    # Convert from pt to inches
    inches_per_pt = 1 / 72.27 
    # Golden ratio to set aesthetic figure height
    # https://disq.us/p/2940ij3
    golden_ratio = (5**.5 - 1) / 2
    # Figure width in inches
    fig_width_in = fig_width_pt * inches_per_pt
    # Figure height in inches
    fig_height_in = fig_width_in * golden_ratio

    fig_dim = (fig_width_in, fig_height_in) 
    return fig_dim
def bracket(ax, pos=[0,0], scalex=1, scaley=1, text="",textkw = {}, linekw = {}):
    x = np.array([0, 0.05, 0.45,0.5])
    y = np.array([0,-0.01,-0.01,-0.02])
    x = np.concatenate((x,x+0.5)) 
    y = np.concatenate((y,y[::-1]))
    ax.plot(x*scalex+pos[0], y*scaley+pos[1], clip_on=False, 
            transform=ax.get_xaxis_transform(), **linekw)
    ax.text(pos[0]+0.5*scalex, (y.min()-0.01)*scaley+pos[1], text, 
            transform=ax.get_xaxis_transform(),
            ha="center", va="top", **textkw)




# Plot Settings 

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


# ## Analytical Duct Mode Solution for Uniform Flow


directories = [
        '../03-EVanalysis/SWIRLVerification/UniformFlowCylinderHardwall/FourthOrderDiff/32pts/',
        '../03-EVanalysis/SWIRLVerification/UniformFlowCylinderHardwall/FourthOrderDiff/64pts/',
        '../03-EVanalysis/SWIRLVerification/UniformFlowCylinderHardwall/FourthOrderDiff/128pts/',
        '../03-EVanalysis/SWIRLVerification/UniformFlowCylinderHardwall/FourthOrderDiff/256pts/',
        '../03-EVanalysis/SWIRLVerification/UniformFlowCylinderHardwall/FourthOrderDiff/512pts/']

NumericalAxialWavenumberData = fcn.importPlotData(
        directories[0] + 'gam.nonconv_acc.0032')

NumericalAxialWavenumberData1 = fcn.importPlotData(
        directories[1] + 'gam.nonconv_acc.0064')


NumericalAxialWavenumberData2 = fcn.importPlotData(
        directories[2] + 'gam.nonconv_acc.0128')

NumericalAxialWavenumberData3 = fcn.importPlotData(
        directories[3] + 'gam.nonconv_acc.0256')

NumericalAxialWavenumberData4 = fcn.importPlotData(
        directories[3] + 'gam.nonconv_acc.0512')

# Input Variables for test case in CodeRun directory
M_x     = 0.3
k       = 10 

# Compute zeros of integer-order Bessel function derivatives Jn'.
m_order = 2 #azimuthal mode

k_x_convection = k / M_x
k_x_cutoff = (k*M_x)/(M_x**2-1)


num_of_zeros = 10 # each zero corresponds to radial modes
# the first zero is radial mode 0
# the second zero is radial mode 1...

x_min   = 0.25
x_max   = 20 
x_steps = 100
x       = np.linspace(x_min,x_max,x_steps)

# Bessel Function Calculation
Jv_p_zero = scip.special.jnp_zeros(n  = m_order, nt = num_of_zeros)
Jv        = scip.special.jv( m_order, x)

fig = plt.figure(
    constrained_layout=False,
    figsize=set_size(width)
)

plt.plot(x,Jv)
plt.plot(Jv_p_zero[0],0,marker='.')
plt.plot(Jv_p_zero[1],0,marker='.')
plt.ylabel('$$ J_{10}(k_r r)$$')
plt.xlabel('$$ k_r r$$')
plt.title('Bessel function of the first kind of order ' + str(m_order))

plt.annotate(
        '$J_{m,\mu}\'(k_r r_{min})$',
        xy=(Jv_p_zero[0],0),
        xycoords='data',
        xytext=(Jv_p_zero[0]+1 ,max(Jv)/1.2),
        textcoords='data',
        arrowprops=dict(
            arrowstyle="->",
            facecolor = 'black',
            )
        )

plt.annotate(
        '$J_{m,\mu}\'(k_r r_{max})$',
        xy=(Jv_p_zero[1],0), xycoords='data',
        xytext=(Jv_p_zero[1] ,0.3), textcoords='data',
        arrowprops=dict(
            arrowstyle="->",
            facecolor = 'black',
            )
        )

plt.savefig(
        fname      ='tex-outputs/analytical_bessel_function.pdf',
        format     ='pdf',
        bbox_inches='tight')



k_x_down = []
k_x_real_down = []
k_x_imag_down = []
k_x_up = []
k_x_real_up = []
k_x_imag_up = []
for i,j in enumerate(Jv_p_zero):
    k_x_up.append(((-M_x*k + cmath.sqrt(k**2 - (1-M_x**2)*Jv_p_zero[i]**2))/(1-M_x**2)))
    k_x_down.append(((-M_x*k - cmath.sqrt(k**2 - (1-M_x**2)*Jv_p_zero[i]**2))/(1-M_x**2)))

    k_x_real_up.append(k_x_up[i].real)
    k_x_imag_up.append(k_x_up[i].imag)
    k_x_real_down.append(k_x_down[i].real)
    k_x_imag_down.append(k_x_down[i].imag)
    
# print(k_x_up)
# print(k_x_down)
fig = plt.figure(
    constrained_layout=False,
#    figsize=set_size(width)
)
plt.axvline(x = k_x_cutoff,color = 'black', label = 'cut-off line',lw=1)
plt.scatter(k_x_real_up,k_x_imag_up,label='Upstream',marker='.')

plt.scatter(k_x_real_down,k_x_imag_down,label='Downstream',marker='.')
# plt.scatter(k_x_convection,0,marker='.',label='$$k_{x,cv}$$')
plt.legend()
plt.xlabel(r'\textit{Real}$(k_x)$')
plt.ylabel(r'\textit{Imaginary}$(k_x)$')

plt.annotate('Cut-Off',
            xy=(k_x_cutoff,0), xycoords='data',
            xytext=(-10 ,-10), textcoords='data',
            arrowprops=dict(
                arrowstyle="->",
                facecolor = 'black',
                 )
            ) 

# plt.annotate('Pure-Convection',
#             xy=(k_x_convection,0), xycoords='data',
#             xytext=(10 ,10), textcoords='data',
#             arrowprops=dict(
#                 arrowstyle="->",
#                 facecolor = 'black',
#                  )
#             )

plt.savefig(
    fname      ='tex-outputs/analytical_solution_test_case_1.pdf',
    format     ='pdf',
    bbox_inches='tight')

matched_wavenumber_real_swirl = []
matched_wavenumber_imag_swirl = []
matched_wavenumber_real = []
matched_wavenumber_imag = []
for ii in range(len(NumericalAxialWavenumberData['Re{gam}'])):
    for jj in range(num_of_zeros):
        # real_match_flag_up = math.isclose(NumericalAxialWavenumberData['Re{gam}'][ii],k_x_real_up[jj],rel_tol = 1e-3)
        # imag_match_flag_up = math.isclose(NumericalAxialWavenumberData['Im{gam}'][ii],k_x_imag_up[jj],rel_tol = 1e-3)
        # real_match_flag_down = math.isclose(NumericalAxialWavenumberData['Re{gam}'][ii],k_x_real_down[jj],rel_tol = 1e-3)
        # imag_match_flag_down = math.isclose(NumericalAxialWavenumberData['Im{gam}'][ii],k_x_imag_down[jj],rel_tol = 1e-3)

#         real_match_flag_up = math.isclose(NumericalAxialWavenumberData['Re{gam}'][ii],k_x_real_up[jj],rel_tol = 1e-2)
#         imag_match_flag_up = math.isclose(NumericalAxialWavenumberData['Im{gam}'][ii],k_x_imag_up[jj],rel_tol = 1e-2)
#         real_match_flag_down = math.isclose(NumericalAxialWavenumberData['Re{gam}'][ii],k_x_real_down[jj],rel_tol = 1e-2)
#         imag_match_flag_down = math.isclose(NumericalAxialWavenumberData['Im{gam}'][ii],k_x_imag_down[jj],rel_tol = 1e-2)

        real_match_flag_up = math.isclose(NumericalAxialWavenumberData['Re{gam}'][ii],k_x_real_up[jj],rel_tol = 1e-1)
        imag_match_flag_up = math.isclose(NumericalAxialWavenumberData['Im{gam}'][ii],k_x_imag_up[jj],rel_tol = 1e-1)
        real_match_flag_down = math.isclose(NumericalAxialWavenumberData['Re{gam}'][ii],k_x_real_down[jj],rel_tol = 1e-1)
        imag_match_flag_down = math.isclose(NumericalAxialWavenumberData['Im{gam}'][ii],k_x_imag_down[jj],rel_tol = 1e-1)


        if real_match_flag_up and imag_match_flag_up == True:
            matched_wavenumber_real.append(k_x_real_up[jj])
            matched_wavenumber_imag.append(k_x_imag_up[jj])
            matched_wavenumber_real_swirl.append(NumericalAxialWavenumberData['Re{gam}'][ii])
            matched_wavenumber_imag_swirl.append(NumericalAxialWavenumberData['Im{gam}'][ii])
            print('SWIRL Output:',NumericalAxialWavenumberData['Re{gam}'][ii],NumericalAxialWavenumberData['Im{gam}'][ii])
            print('Analytical  :',k_x_real_up[jj],k_x_imag_up[jj])

        if real_match_flag_down and imag_match_flag_down == True:
            matched_wavenumber_real.append(k_x_real_down[jj])
            matched_wavenumber_imag.append(k_x_imag_down[jj])
            print('SWIRL Output:',NumericalAxialWavenumberData['Re{gam}'][ii],NumericalAxialWavenumberData['Im{gam}'][ii])
            print('Analytical  :',k_x_real_down[jj],k_x_imag_down[jj])
            matched_wavenumber_real_swirl.append(NumericalAxialWavenumberData['Re{gam}'][ii])
            matched_wavenumber_imag_swirl.append(NumericalAxialWavenumberData['Im{gam}'][ii])
wvnSum = 0
# print(matched_wavenumber_real)
# print(matched_wavenumber_real_swirl)
for ii in range(len(matched_wavenumber_real)):
    print(matched_wavenumber_real[ii])
    print(matched_wavenumber_real_swirl[ii])
    wvn_error = abs(matched_wavenumber_real[ii] - matched_wavenumber_real_swirl[ii])
    # print(ii,wvn_error)
    wvn_error_squared = wvn_error**2 
    wvnSum = wvnSum + wvn_error_squared

if wvnSum >= 10e-12:
    wvn_L2 = math.sqrt(wvnSum/len(matched_wavenumber_imag))
    print("real L2 {:0.12f}".format(wvn_L2))
else:
    print('L2 is below machine precision')
wvnSum = 0
for ii in range(len(matched_wavenumber_imag)):
    print(matched_wavenumber_imag[ii])
    print(matched_wavenumber_imag_swirl[ii])
    wvn_error = abs(matched_wavenumber_imag[ii] - matched_wavenumber_imag_swirl[ii])
    # print(ii,wvn_error)
    wvn_error_squared = wvn_error**2 
    wvnSum = wvnSum + wvn_error_squared

print("{:0.12f}".format(wvnSum))

if wvnSum >= 10e-12:
    wvn_L2 = math.sqrt(wvnSum/len(matched_wavenumber_imag))
    print("imag L2 {:0.12f}".format(wvn_L2))
else:
    print('L2 is below machine precision')


   
    
plt.scatter(matched_wavenumber_real_swirl,matched_wavenumber_imag_swirl,facecolor = 'none', edgecolors ='b', marker = 'd')
plt.savefig(
    fname      ='tex-outputs/analytical_solution_test_case_1_comparison.pdf',
    format     ='pdf',
    bbox_inches='tight')



plt.show()



# Notes: 
# np arrays would be faster than lists
