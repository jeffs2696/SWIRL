#!/usr/bin/env python3
import pprint
import pandas
import sys 
import math
import cmath
import numpy as np
import scipy as scp
from scipy import special
import helpers.importing_functions as ifcn
import helpers.math_functions as mfcn
import helpers.mode_sorting_functions as msfcn
import helpers.analytic_functions as afcn
import helpers.plotting_functions as pfcn 
import helpers.helper_functions as fcn
import matplotlib as mpl
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D # needed for 3d plots
from matplotlib import cm

from inputs.test_case_variables import TestCase1 

# this used to be in main but its cool stuff 

def main():

    # test case parameters
    
    num_of_zeros = 5 # each zero corresponds to radial modes
    wavenumber = 10 
    azimuthal_mode_number = 2 
    radial_mode_number = 0
    axial_mach_number = 0.3
    r_min = 0
    r_max = 1
    r_steps = 100
    
    r = np.linspace(r_min,r_max,r_steps)
                                                                       
    Jv_p_zero = afcn.get_radial_wavenumbers(
            azimuthal_mode_number,
            num_of_zeros)
                                                                       
    Jv_p_zero_01 = scp.special.jnp_zeros(
            n  = 0, nt = 1) 
                                                                       
    # Get axial wavenumbers for a given temporal and radial wavenumber
    k_x_plus,k_x_minus = afcn.get_axial_wavenumbers(
            azimuthal_mode_number,
            wavenumber,
            axial_mach_number,
            Jv_p_zero) 
                                                                       
    radial_mode_list = afcn.get_radial_modes(
            azimuthal_mode_number,
            Jv_p_zero,
            r) 
    
    # normalize radial modes numerically vs using analytics from Rienstra 
    # Note: NASA's radial mode index starts at 0 while Rienstra starts at 1
    # This depend on whether you count the first 0 at the boundary or not..  
    
    normalized_radial_mode_list, \
    analytical_normalized_radial_mode_list = \
    afcn.normalize_radial_modes(
            r,
            radial_mode_list,
            radial_mode_number,
            azimuthal_mode_number,
            Jv_p_zero) 

    radial_mode = radial_mode_list[0] 
    normalized_radial_mode = normalized_radial_mode_list[0]
    analytical_normalized_radial_mode = analytical_normalized_radial_mode_list[0]
    fig = plt.figure()
    
    plt.plot(r,radial_mode)
    

    plt.plot(r,normalized_radial_mode,label='Normalized - Numerical')

    plt.plot(r,analytical_normalized_radial_mode,label='Normalized - Analytic',linestyle='dotted',lw=2,marker='o')

    plt.ylabel(r'\textit{Pressure,}$(p)$') 


    sanity_check_for_normalization =np.trapz(
            np.conj(normalized_radial_mode)*normalized_radial_mode*r,r)

    sanity_check_for_analytical_normalization =np.trapz(
            np.conj(analytical_normalized_radial_mode)*analytical_normalized_radial_mode*r,r)

    print('Integral of Normalized Modes')
    print('Numerical',sanity_check_for_normalization)
    print('Analytic ',sanity_check_for_analytical_normalization)

    txt = ("Analytical Propagating Mode for " + 
            "$m = {M}$," + 
            "$k_r = {N}$,"+
            "$k = {freq}$,"+
            "$M_x = {speed}$,"+ 
            "$\eta = {liner}$").format(
                    M     = azimuthal_mode_number,
                    N     = radial_mode_number, 
                    freq  = wavenumber ,
                    speed = axial_mach_number,
                    liner = 0)

    plt.xlabel(r'\begin{center}\textit{Radius,}$(r)$\\*\textit{\small{' + txt + r'}}\end{center}')
    # plt.figtext(k_x_plus[0],k_x_minus[0],txt, wrap = True, horizontalalignment='center', fontsize=12)
    plt.legend()
    fig.set_size_inches(10, 8, forward=True)


    plt.savefig(
            fname      ='figures/Radial_mode_analytical_test_case.pdf',
            format     ='pdf',
            bbox_inches='tight')


    # plt.show()
    # sys.exit()


    # 3D plots
    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')
    # ax.plot_surface(angle,x,mode_solution)
    # plt.show()

    # Create the mesh in polar coordinates and compute corresponding Z.
    r = np.linspace(0, 1, 50)
    p = np.linspace(0, 2*np.pi, 50)
    # Function would start here
    R, P = np.meshgrid(r, p)

    radial_mode = scp.special.jv(
            azimuthal_mode_number,
            Jv_p_zero[radial_mode_number]*R)

    Z = radial_mode*np.exp(-1j*azimuthal_mode_number*P).real

    # Express the mesh in the cartesian system.
    X, Y = R*np.cos(P), R*np.sin(P)

    # Plot the surface.
    ax.plot_surface(X, Y, Z, cmap=plt.cm.YlGnBu_r)
    ax.view_init(30,90)
    filename ='Radial_mode_analytical_test_case_3d1'
    plt.savefig(
            fname      ='figures/'+filename+'.pdf',
            format     ='pdf',
            bbox_inches='tight')

    ax = plt.figure().add_subplot(projection='3d')

    ax.plot_surface(X, Y, Z, cmap=plt.cm.YlGnBu_r)

    # Plot projections of the contours for each dimension.  By choosing offsets
    # that match the appropriate axes limits, the projected contours will sit on
    # the 'walls' of the graph.
    ax.contourf(X, Y, Z, zdir='z', offset=(max(Z[0])+max(Z[0])/2),cmap =plt.cm.YlGnBu_r)#, cmap=cm.coolwarm)
    # need color bar

    ax.set(
            xlabel='X', ylabel='Y', zlabel='P')

    plt.savefig(
            fname      ='figures/Radial_mode_analytical_test_case_3d2.pdf',
            format     ='pdf',
            bbox_inches='tight')

    animate_flag = False
    # if animate_flag:
    #     # rotate the axes and update
    #     # for angle in range(0, 360):
    #     ax.view_init(30, angle)
    #     plt.draw()
    #     plt.pause(.01)
    #     plt.show()


    points = 1000

    angle_min = 0 
    angle_max = 360*2 
    dangle = angle_max - angle_min
    n = dangle/720
    r = 1
    # pitch -distance from curve to curve
    c = 2*np.pi*azimuthal_mode_number/abs(k_x_plus[0].real)
    # c =1# 2*np.pi*azimuthal_mode_number/abs(k_x_plus[0].real)
    # print('n',n)
    u = np.linspace(0, r, endpoint=True, num=int(points * n))
    v = np.linspace(-np.deg2rad(angle_min), np.deg2rad(angle_max), endpoint=True, num=int(2 * points * n))
    u, v = np.meshgrid(u, v)

    x = u * np.cos(v)   # r*cos(theta)
    y = u * np.sin(v)   # r*cos(theta)

    # JS:
    #pitch*theta + r*arctan(phase)
    #phase = m*theta + Re(k_x)*x
    z = c * v + u*np.arctan(azimuthal_mode_number*v + k_x_plus[0].real*u) 



    fig = plt.figure()
    ax = fig.add_subplot(111, projection='3d')

    ax.plot_surface(z, x, y)#, alpha=1, edgecolors='w', zorder=0, color='black')
    # ax.quiver(-6, 0, 0, 24, 0, 0, length=1, arrow_length_ratio=0.03, color='black', zorder=1)
    # ax.text(19, -2, -2, "$\mathbf{z}$", fontsize=40, color='black', zorder=1)

    plt.savefig(
            fname      ='figures/helicoid_test_case_3d2.pdf',
            format     ='pdf',
            bbox_inches='tight')

    plt.show()

if __name__ == '__main__':
    main()
