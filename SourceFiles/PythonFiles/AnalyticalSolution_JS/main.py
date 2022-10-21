#!/usr/bin/env python3

# from classes.DuctModeClass import DuctModeClass
from classes import DuctModeClass
# import logging
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

def main(): 
    
    fcn.set_plot_parameters() #plot parameters

    # test case parameters
    
    num_of_zeros = 5 # each zero corresponds to radial modes
    wavenumber = 10 
    azimuthal_mode_number = 2 
    axial_mach_number = 0.3
    r_min = 0
    r_max = 1

    # grid_point_array = np.array([32, 64, 128,256]) 

    grid_point_array = np.array([33, 66, 132,264,528,1056]) 
    second_order_directories, fourth_order_directories, \
            NumericalAxialWavenumberData_second_order_list, \
            NumericalAxialWavenumberData_fourth_order_list = \
            ifcn.importNumericalWavenumbers(grid_point_array)
    # print(NumericalAxialWavenumberData_second_order_list[0].info()) 

    ROC_nested_list = []
    for i_rad_mode_num in range(num_of_zeros):

        radial_mode_iteration_string = 'Radial Mode Number ' + \
        str(i_rad_mode_num)

        downstream_L2 = []
        upstream_L2 = []
        ROC_list = []

        # radial mode number loop
        radial_mode_number = i_rad_mode_num
    
        print(f'{radial_mode_iteration_string: ^20}')
    
        NumericalAxialWavenumberData_list = NumericalAxialWavenumberData_fourth_order_list

    
        for i_gp,j_gp in enumerate(grid_point_array): 

            r_steps = grid_point_array[i_gp]

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
            
            m_dict = msfcn.sortAxialWavenumbers(
                    axial_mach_number,
                    wavenumber,
                    num_of_zeros,
                    NumericalAxialWavenumberData_list[i_gp],
                    grid_point_array)

            k_x_numerical = m_dict['k_x'] 

# TODO: plot_numerical_axial_wavenumber(m_dict):

            radial_mode_filenames = []
    
            for i in range(len(m_dict['radial_mode_index'])):
    
                if i_gp == 0:
                    if m_dict['radial_mode_index'][i] < 100:
                        radial_mode_filenames.append(
                                'egv_np_00{n}radialmode_00'.format(
                            n = j_gp) +
                                str(m_dict['radial_mode_index'][i])) 
                    else:
                        radial_mode_filenames.append(
                                'egv_np_00{n}radialmode_0'.format(
                            n = j_gp ) +
                                str(m_dict['radial_mode_index'][i]))
    
                if i_gp == 1:
                    if m_dict['radial_mode_index'][i] < 100:
                        radial_mode_filenames.append('egv_np_00{n}radialmode_00'.format(
                            n = j_gp)+ str(m_dict['radial_mode_index'][i])) 
                    else:
                        radial_mode_filenames.append('egv_np_00{n}radialmode_0'.format(
                            n = j_gp) + str(m_dict['radial_mode_index'][i]))

                if i_gp == 2:
                    if m_dict['radial_mode_index'][i] < 100:
                        radial_mode_filenames.append('egv_np_0{n}radialmode_00'.format(
                            n = j_gp) + str(m_dict['radial_mode_index'][i])) 
                    else:
                        radial_mode_filenames.append('egv_np_0{n}radialmode_0'.format(
                            n = j_gp) + str(m_dict['radial_mode_index'][i]))
                if i_gp == 3:
                    if m_dict['radial_mode_index'][i] < 100:
                        radial_mode_filenames.append('egv_np_0{n}radialmode_00'.format(
                            n = j_gp) + str(m_dict['radial_mode_index'][i])) 
                    else:
                        radial_mode_filenames.append('egv_np_0{n}radialmode_0'.format(
                            n = j_gp) + str(m_dict['radial_mode_index'][i]))
                if i_gp == 4:
                    if m_dict['radial_mode_index'][i] < 100:
                        radial_mode_filenames.append('egv_np_0{n}radialmode_00'.format(
                            n = j_gp) + str(m_dict['radial_mode_index'][i])) 
                        
                    elif m_dict['radial_mode_index'][i] < 1000:
                        radial_mode_filenames.append('egv_np_0{n}radialmode_0'.format(
                            n = j_gp) + str(m_dict['radial_mode_index'][i]))
                    else:
                        radial_mode_filenames.append('egv_np_0{n}radialmode_'.format(
                            n = j_gp) + str(m_dict['radial_mode_index'][i]))

                if i_gp == 5:
                    if m_dict['radial_mode_index'][i] < 100:
                        radial_mode_filenames.append('egv_np_{n}radialmode_00'.format(
                            n = j_gp) + str(m_dict['radial_mode_index'][i])) 
                    elif m_dict['radial_mode_index'][i] < 1000:
                        radial_mode_filenames.append('egv_np_{n}radialmode_0'.format(
                            n = j_gp) + str(m_dict['radial_mode_index'][i]))
                    else: 
                        radial_mode_filenames.append('egv_np_{n}radialmode_'.format(
                            n = j_gp) + str(m_dict['radial_mode_index'][i]))
                    
            radial_mode_dataframe_dictionary = {}
    
            for i in range(len(m_dict['radial_mode_index'])):
                radial_mode_data = \
                        pandas.read_csv(  fourth_order_directories[i_gp] + radial_mode_filenames[i], delim_whitespace = True )
                # print(j_gp)
                
                radial_mode_dataframe_dictionary[i] = radial_mode_data
        # plotting data 
    
        
            fig,ax = plt.subplots(
                    constrained_layout=True)
    
            scatter_parameters = {
                    'marker':'.' ,
                    'c':'black'}
            
            scatter_parameters_numerical = {
                    'marker':'.' ,
                    'c':'blue'}
            
            for i in range(num_of_zeros):
                ax = pfcn.plot_axial_wavenumbers(
                        real_part = k_x_plus[i].real,
                        imag_part = k_x_plus[i].imag,
                        azimuthal_mode_number = azimuthal_mode_number,  
                        radial_mode_number = i,
                        wavenumber = wavenumber ,
                        axial_mach_number = axial_mach_number,
                        scatter_kwargs = scatter_parameters
                        )
            
                ax = pfcn.plot_axial_wavenumbers(
                        real_part = k_x_minus[i].real,
                        imag_part = k_x_minus[i].imag,
                        azimuthal_mode_number = azimuthal_mode_number,  
                        radial_mode_number = i,
                        wavenumber = wavenumber ,
                        axial_mach_number = axial_mach_number,
                        scatter_kwargs = scatter_parameters
                        )
            
            if axial_mach_number > 0:        
                k_x_cutoff = axial_mach_number*wavenumber/(axial_mach_number**2-1)
                plt.axvline(x = k_x_cutoff,color = 'black', label = 'cut-off line',lw=0.5,ls='dotted') 
        
    
            # sorting modes, using the max and min real part

            
            for i in range(len(m_dict['k_x'])):
                
                plt.scatter(
                        k_x_numerical[i][0], # real part
                        k_x_numerical[i][1], # imaginary part
                        marker = 'd',# changes triangle direction
                        # label = ii,
                        facecolor ='none', edgecolors ='b',
                        )
                if k_x_numerical[i][1] > 0 or k_x_numerical[i][1] < 0: 
                    ax.annotate(r'$K_{{{M},{N}}}$'.format(
                        M = azimuthal_mode_number,
                        N = m_dict['radial_mode_number'][i]),
                        xy=(
                            m_dict['k_x'][i][0],
                            m_dict['k_x'][i][1]),
                        xycoords='data',
                        textcoords='offset points',
                        xytext = (-20,-20),
                        horizontalalignment='center',
                        verticalalignment='bottom',
                        fontsize='10',
                        arrowprops=dict(arrowstyle= '-',
                            color='blue',
                            lw=1,
                            ls='--')
                        )
                else: 
                    ax.annotate(r'$K_{{{M},{N}}}$'.format(
                        M = azimuthal_mode_number,
                        N = m_dict['radial_mode_number'][i]),
                        xy=(
                            m_dict['k_x'][i][0],
                            m_dict['k_x'][i][1]),
                        xycoords='data',
                        textcoords='offset points',
                        xytext = (0,-20),
                        horizontalalignment='center',
                        verticalalignment='center',
                        fontsize='10',
                        arrowprops=dict(arrowstyle= '-',
                            color='blue',
                            lw=1,
                            ls='--') 
                        )

            plt.savefig( 
                    fname      ='figures/fourth_order_wavenumber_grid_{n}.pdf'.format(
                            n = str(grid_point_array[i_gp])),
                        format     ='pdf')
            # plt.show()
            # sys.exit() 

            dict_indicies = []
            numerical_normalized_radial_mode_list = []
            error_list = []
            L2_list = []
            L_max_list = []
            L_max_location_list = [] 

            for i,j in enumerate(m_dict['radial_mode_number']):
                if j == i_rad_mode_num:
                    # print('match!',j,'at',i)
                    dict_indicies.append(i)
            
            # test = [t for t,x in enumerate(mode_dictionary['radial_mode_index'][t]) if x == radial_mode_number]# in mode_dictionary['radial_mode_index']:
            # print('test',test) 

            for i in range(len(dict_indicies)):
                dict_index = dict_indicies[i]
                numerical_r = radial_mode_dataframe_dictionary[dict_index]['Rad'] 
    
                numerical_radial_mode_real = radial_mode_dataframe_dictionary[dict_index]['p_no_phase[Re]']
    
                numerical_radial_mode_imag = radial_mode_dataframe_dictionary[dict_index]['p_no_phase[Im]']
    
                numerical_radial_mode = numerical_radial_mode_real.to_numpy() + numerical_radial_mode_imag.to_numpy()*1j
    
                normalization_constant_numerical = afcn.normalize_psi((numerical_radial_mode),numerical_r) 
    
                normalized_radial_mode_data = normalization_constant_numerical*numerical_radial_mode
                
                numerical_normalized_radial_mode_list.append(normalized_radial_mode_data)
    
                # for i in range(len(dict_indicies)):
        
                error_list.append(abs(numerical_normalized_radial_mode_list[i][:].real-
                    analytical_normalized_radial_mode_list[radial_mode_number][:].real))
    
                # error_list.append(abs(numerical_normalized_radial_mode_list[i][1:-2].real-
                #     analytical_normalized_radial_mode_list[radial_mode_number][1:-2].real)) 
            
                    # L2 = mfcn.getL2norm(
                    #         numerical_normalized_radial_mode_list[i][:].real,
                    #         analytical_normalized_radial_mode_list[radial_mode_number][:].real )
                    
    
                L2 = np.sqrt((1/grid_point_array[i_gp])*sum(error_list[i]**2))
                # two L2s one for upstream and downstream mode
                # print(L2)
                # sys.exit()

                L2_list.append(L2)
                
                L_max = max(error_list[i]) 
                L_max_list.append(L_max)
                L_max_location = np.argmax(error_list[i])
                L_max_location_list.append(L_max_location)
                # L_max_location =error_list[i].index(L_max) 
                
                # print('L_max',L_max_list[i])
                # print('L_max_location',L_max_location_list[i])
    
                # print(L2_list)
                # for i in range(len(dict_indicies)):
                dict_index = dict_indicies[i]
                fig,ax = plt.subplots(
                        constrained_layout=False,figsize=fcn.set_size(345)
                        ) 
                plt.plot(
                        numerical_r,
                        normalized_radial_mode_data.real,
                        label = 'numerical,real')
                
                plt.plot(
                        numerical_r,
                        normalized_radial_mode_data.imag,
                        label = 'numerical,imag')
                
                plt.title('Radial mode '+ str(m_dict['radial_mode_number'][dict_index]))
                plt.suptitle(str(m_dict['k_x'][dict_index]))
                
                plt.plot(r,normalized_radial_mode_list[radial_mode_number].real, label='analytic,real',linestyle='dotted')
                plt.plot(r,normalized_radial_mode_list[radial_mode_number].imag, label='analytic,imag',linestyle='dotted')
                plt.legend()
                plt.xlabel('Radius')
                plt.ylabel('Pressure Fluctuation')
                # print('gp',j_gp)
                plt.savefig(
                        fname      ='figures/fourth_order_radial_mode_{N}_test_case_number_{index}_grid_{n}.pdf'.format(
                            N = radial_mode_number,
                            index = i,
                            n = str(grid_point_array[i_gp])),
                        format     ='pdf')
                
                fig,ax = plt.subplots()
                
                plt.plot(error_list[i])
                plt.savefig(
                        fname      ='figures/fourth_order_radial_mode_error_{N}_test_case_number_{index}_grid_{n}.pdf'.format(
                            N = radial_mode_number,
                            index = i,
                            n = str(grid_point_array[i_gp])),
                        format     ='pdf')
                
            downstream_L2.append(L2_list[0]) 
            upstream_L2.append(L2_list[1])
                
        for i in range(len(downstream_L2)-1): 

            ROC = np.log(downstream_L2[i+1]/downstream_L2[i])/\
                    np.log(grid_point_array[i]/grid_point_array[i+1])
                    
            # print((grid_point_array[i]/grid_point_array[i+1]))
            
            ROC_list.append(ROC)
            # np.save('ROCdata.dat',ROC)
            # with open(r'ROC_result.dat','w') as rocf:
                # rocf.write('\n'.join(str(ROC_list)))
        print(ROC_list)
    ROC_nested_list.append(ROC_list)
        # print(ROC_list)
            # list_list = L2_list[::2] 
            # print(list_list)
            # print(L2_list)
            # plot Swirl's radial modes ...
                
            # list_list.append(L2_list[0]) 
            # print(list_list)
            # print(list_list[::2])
            # list_list = list_list[::2]
    # for i in range(len(list_list)-1):

            # ROC = np.log(list_list[i+1]/list_list[i])/(grid_point_array[i+1]-grid_point_array[i])
            # print(ROC)
            # print((grid_point_array[i]/grid_point_array[i+1]))
        #   # ROC = ist_list[i][1]


    # plt.show()
    # plt.close()
    # plt.plot(L2_list)
    # plt.show()
    # fig,ax = plt.subplots()
    #upstream modes
    # plt.plot(list_list[:])
    # plt.show()

    # print(L2_list)
    # print(L_max_list)
    # print(list_list[::2][1])
    # print(list_list)
    # list_list = list_list[:]

    # print('upstream',list_list[:][0][::2])
    # print((list_list[1][:][:]))


    # CODE ENDS HERE

    # plt.show()
    sys.exit()
    # resize the figure to match the aspect ratio of the Axes    
    # fig.set_size_inches(10, 8, forward=True)

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
