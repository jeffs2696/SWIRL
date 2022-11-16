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

    # obtaining axial wavenumber outputs for all grids in study
    second_order_directories, fourth_order_directories, \
            NumericalAxialWavenumberData_second_order_list, \
            NumericalAxialWavenumberData_fourth_order_list = \
            ifcn.importNumericalWavenumbers(grid_point_array)


    ROC_nested_list = []

    for i_rad_mode_num in range(num_of_zeros):

        radial_mode_iteration_string = 'Radial Mode Number ' + \
        str(i_rad_mode_num)

        downstream_L2 = []
        downstream_Lmax = []
        upstream_L2 = []
        upstream_Lmax = []
        r_min_L2   = []
        ROC_list = []
        ROC_L_max_list = []

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

            #
            k_x_numerical = m_dict['k_x'] 

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
                            N = J_gp) + str(m_dict['radial_mode_index'][i])) 
                    elif m_dict['radial_mode_index'][i] < 1000:
                        radial_mode_filenames.append('egv_np_{n}radialmode_0'.format(
                            n = j_gp) + str(m_dict['radial_mode_index'][i]))
                    else: 
                        radial_mode_filenames.append('egv_np_{n}radialmode_'.format(
                            n = j_gp) + str(m_dict['radial_mode_index'][i]))
                    
            radial_mode_dataframe_dictionary = {}
    
            for i in range(len(m_dict['radial_mode_index'])):
                radial_mode_data = \
                        pandas.read_csv(  fourth_order_directories[i_gp] +
                                radial_mode_filenames[i], delim_whitespace = True )
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
        
                error_list.append(abs(numerical_normalized_radial_mode_list[i][:].real-
                    analytical_normalized_radial_mode_list[radial_mode_number][:].real))
    
                # two L2s one for upstream and downstream mode
                L2 = mfcn.getL2norm(
                        numerical_normalized_radial_mode_list[i][:].real,
                        analytical_normalized_radial_mode_list[radial_mode_number][:].real )

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
            downstream_Lmax.append(L_max_list[0]) 
            
            upstream_L2.append(L2_list[1])
            upstream_Lmax.append(L_max_list[1])

                
        # df = open('fourth_ourder_convergenceRates_radial_mode_{}'.format(str(i_rad_mode_num)),'w')
        # df.write('{},{},{},{}'.format('ROC','ROC_at_Lmax','Lmaxlocation','iter'))
        for i in range(len(downstream_L2)-1): 
            ROC = np.log(downstream_L2[i+1]/downstream_L2[i])/\
                    np.log(grid_point_array[i]/grid_point_array[i+1])
                    
            ROC_for_L_max = np.log(downstream_Lmax[i+1]/downstream_Lmax[i])/\
                    np.log(grid_point_array[i]/grid_point_array[i+1])


            # df.write('{},{},{},{}'.format(ROC,ROC_for_L_max,L_max_location_list[i],i))
            # df.write('\n')
            
            ROC_list.append(ROC)
            ROC_L_max_list.append(ROC_for_L_max)
            ROC_nested_list.append(ROC_list)

    print(ROC_list)

if __name__ == '__main__':
    main()
