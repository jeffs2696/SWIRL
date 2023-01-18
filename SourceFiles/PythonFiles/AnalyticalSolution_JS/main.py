#!/usr/bin/env python3

# from classes.DuctModeClass import DuctModeClass
from classes import DuctModeClass
import logging
import pprint
import pandas
import sys 
import math
import cmath
import numpy as np
import scipy as scp
from scipy import special
import helpers.logging_functions as lfcn
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

# get logging settings 
logger = lfcn.log_to_console()

# used to print indents to log
tab = '--'  

def main(): 
    
    logger.info(
            f"{'Validation Study for Uniform Flow in Cylindrical Ducts':=^90}")


    debug = True
    debug_per_mode = True
    plot_figures_flag = False

    fcn.set_plot_parameters() #plot parameters

    # test case parameters
    
    
    num_of_zeros = 2 # each zero corresponds to radial modes
    wavenumber = 10 
    azimuthal_mode_number = 2 
    axial_mach_number = 0.3
    r_min = 0
    r_max = 1
    grid_point_array = np.array([33, 66])#, 132, 264,528,1056])
    # grid_point_array = np.array([32, 64, 128,256]) 
    # grid_point_array = np.array([33,66,132, 264,528,1056]) 
    number_of_grids = len(grid_point_array)

    if debug:
        logger.info(f'Nondimensional Inputs:')
        logger.info(f' Wavenumber, k            : {wavenumber: <10}')
        logger.info(f' Azimuthal Mode Number, m : {azimuthal_mode_number: <10}')
        logger.info(f' Axial Mach Number, M_x   : {axial_mach_number: <10}')
        logger.info(f' Radial Minimum, r_min    : {r_min: <10}')
        logger.info(f' Radial Minimum, r_min    : {r_max: <10}')
        logger.info(f' Number of Radial Modes   : {num_of_zeros: <10}')
        logger.info(f' Number of Grids          : {number_of_grids: <10}')
        

    # obtaining axial wavenumber outputs for all grids in study

    if debug:
        logger.info(f'Importing SWIRL data')

    second_order_directories, \
            fourth_order_directories, \
            NumericalAxialWavenumberData_second_order_list, \
            NumericalAxialWavenumberData_fourth_order_list = \
            ifcn.importNumericalWavenumbers(grid_point_array)
            
    # obtaining mode data

    m_dict_second_order = []
    m_dict_fourth_order = [] 
    radial_mode_filenames = []

    if debug:
        logger.info(f'Sorting SWIRL data')


    for i_gp,j_gp in enumerate(grid_point_array): 

        if debug:
            logger.info(f'{tab*2}' \
                    f' Grid : {i_gp:>5}' \
                    f'{j_gp:>5} points')


        m_dict_second_order.append(msfcn.sortAxialWavenumbers(
                axial_mach_number,
                wavenumber,
                num_of_zeros,
                NumericalAxialWavenumberData_second_order_list[i_gp],
                grid_point_array))

        logger.info(f'Second Order\n' + 
            pprint.pformat(m_dict_second_order[i_gp]))

        m_dict_fourth_order.append(msfcn.sortAxialWavenumbers(
            axial_mach_number,
            wavenumber,
            num_of_zeros,
            NumericalAxialWavenumberData_fourth_order_list[i_gp],
            grid_point_array))

        logger.info(f'Fourth Order\n' + 
            pprint.pformat(m_dict_second_order[i_gp]))
        # pprint.pprint(m_dict_fourth_order)

    # pprint.pprint(m_dict_fourth_order)
    # logger.info(pprint.pprint(m_dict_fourth_order)
    
        m_dict = m_dict_fourth_order
        for i in range(len(m_dict['radial_mode_index'])):
            # this concept is flawed .... it expects a certain array of grid points
            #
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
                    

            logger.info(f'{radial_mode_filenames}')
            radial_mode_dataframe_dictionary = {} 
    
            for i in range(len(m_dict['radial_mode_index'])):
                radial_mode_data = \
                        pandas.read_csv(  fourth_order_directories[i_gp] +
                                radial_mode_filenames[i], delim_whitespace = True )
                        # print(j_gp) 
                radial_mode_dataframe_dictionary[i] = radial_mode_data
    
    #sys.exit()


    ROC_nested_list = []

    for i_rad_mode_num in range(num_of_zeros):

        downstream_L2 = []
        downstream_L2_without_left = []
        downstream_L2_without_right = []
        downstream_L2_without_boundaries = []
        downstream_Lmax = []
        downstream_Lmax_location = []
        upstream_L2_without_left = []
        upstream_L2_without_right = []
        upstream_L2_without_boundaries = []
        upstream_L2 = []
        upstream_Lmax = []
        upstream_Lmax_location = []
        r_min_L2   = []
        ROC_axial_wavenumber_downstream_list = []
        ROC_axial_wavenumber_upstream_list = []
        ROC_downstream_list = []
        ROC_kx_downstream_list = []
        ROC_kx_upstream_list = []
        ROC_downstream_without_left_list = []
        ROC_downstream_without_right_list = []
        ROC_downstream_without_boundaries_list = []
        ROC_upstream_list = []
        ROC_upstream_without_left_list = []
        ROC_upstream_without_right_list = []
        ROC_upstream_without_boundaries_list = []
        ROC_L_max_downstream_list = []
        ROC_L_max_upstream_list = []
        kx_downstream_list = []
        kx_upstream_list = []
        kx_downstream_error = []
        kx_upstream_error = []
        

        # radial mode number loop
        radial_mode_number = i_rad_mode_num
    
        if debug_per_mode:
            logger.info(f'{tab} Radial Mode Number : {i_rad_mode_num: <5}')
    
        # set list second or fourth order
        NumericalAxialWavenumberData_list = NumericalAxialWavenumberData_fourth_order_list
    
        for i_gp,j_gp in enumerate(grid_point_array): 
            if debug_per_mode:
                logger.info(f'{tab*2}' \
                        f' Grid : {i_gp:>5}' \
                        f'{j_gp:>5} points')

            # Compute analytics
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

            # Sort Axial Wavenumbers by limiting the damping rate 
            # (see helpers/mode_sorting_functions.py)

            m_dict = msfcn.sortAxialWavenumbers(
                    axial_mach_number,
                    wavenumber,
                    num_of_zeros,
                    NumericalAxialWavenumberData_list[i_gp],
                    grid_point_array)

            if debug:
                pprint.pprint(m_dict)
            #
            k_x_numerical = m_dict['k_x'] 

            radial_mode_filenames = []
    
            for i in range(len(m_dict['radial_mode_index'])):
                # this concept is flawed .... it expects a certain array of grid points
                # 
    
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
    
        
            if plot_figures_flag:
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
                plt.show()

            dict_indicies = []
            numerical_normalized_radial_mode_list = []
            error_list = []
            L2_list = []
            L2_list_without_left = []
            L2_list_without_right = []
            L2_list_without_boundaries = []
            
            L_max_list = []
            L_max_location_list = [] 


# Reminder: the radial mode number in m_dict is the number of identified zeros from SWIRL!
# This creates a new dictionary that appends the dictionaries for each mode in SWIRL, 
# so that only the relevant ones are saved 

            # logger.info(f'Finding matches in numerical data...')
            # logger.info(f'Comparing radial mode numbers...')
            # logger.info(f'Looking for two modes, upstream and downstream')

            for i,j in enumerate(m_dict['radial_mode_number']):
                if j == i_rad_mode_num:
                    # print('   match! ',j,' identified mode index',i)
                    dict_indicies.append(i)
            
            # test = [t for t,x in enumerate(mode_dictionary['radial_mode_index'][t]) if x == radial_mode_number]# in mode_dictionary['radial_mode_index']:
            # print('test',test) 

# JS: I feel like this loop is here for no reason, and can be combined with 
# the one uptop
            for i in range(len(dict_indicies)):

                corresponding_axial_wavenumber =m_dict['k_x'][i] 

                if debug_per_mode:
                    logger.info(f'{tab*3} Identified Mode : {i: <10}') 
                    logger.info(
                            f'{tab*4}' +
                            f' Axial Wavenumber         : '+
                            f'{corresponding_axial_wavenumber[0]  : >.11e} ' +
                            f'{corresponding_axial_wavenumber[1]  : >.11e}j ')

                
                mode_direction = np.sign(corresponding_axial_wavenumber[0]) # real part only...

                if (mode_direction) == 1.0: 
                    kx_downstream_list.append(corresponding_axial_wavenumber)
                    if debug_per_mode:
                        logger.info(f'{tab*4} Downstream Propagation')
                elif (mode_direction) == -1.0:
                    kx_upstream_list.append(corresponding_axial_wavenumber)
                    if debug_per_mode:
                        logger.info(f'{tab*4} Upstream Propagation')

                dict_index = dict_indicies[i]
                numerical_r = radial_mode_dataframe_dictionary[dict_index]['Rad']

                numerical_radial_mode_real = radial_mode_dataframe_dictionary[dict_index]['p_no_phase[Re]']

                numerical_radial_mode_imag = radial_mode_dataframe_dictionary[dict_index]['p_no_phase[Im]']

                numerical_radial_mode = numerical_radial_mode_real.to_numpy() + numerical_radial_mode_imag.to_numpy()*1j

                normalization_constant_numerical = afcn.normalize_psi((numerical_radial_mode),numerical_r)

                normalized_radial_mode_data = normalization_constant_numerical*numerical_radial_mode

                numerical_normalized_radial_mode_list.append(normalized_radial_mode_data)

                # print('r mode, grid #, index for identified mode')
                # print(i_rad_mode_num,i_gp,i)

                mode_error =abs(
                            numerical_normalized_radial_mode_list[i][:].real-
                            analytical_normalized_radial_mode_list[radial_mode_number][:].real)  

                # a little redundant but I want this data (redundant for L2 calc)
                error_list.append(
                        mode_error
                        )
    
                # two L2s one for upstream and downstream mode
                # kx_downstream_error.append(
                        # abs(kx_downstream_list[i][0] - k_x_plus[0].real)) 
                # kx_upstream_error.append(
                        # abs(kx_upstream_list[i][0] - k_x_minus[0].real))

                L2 = mfcn.getL2norm(
                        numerical_normalized_radial_mode_list[i][:].real,
                        analytical_normalized_radial_mode_list[radial_mode_number][:].real )

                L2_without_leftboundary = mfcn.getL2norm(
                        numerical_normalized_radial_mode_list[i][1:].real,
                        analytical_normalized_radial_mode_list[radial_mode_number][1:].real)

                L2_without_rightboundary = mfcn.getL2norm(
                        numerical_normalized_radial_mode_list[i][:-1].real,
                        analytical_normalized_radial_mode_list[radial_mode_number][:-1].real)

                L2_without_boundary = mfcn.getL2norm(
                        numerical_normalized_radial_mode_list[i][1:-1].real,
                        analytical_normalized_radial_mode_list[radial_mode_number][1:-1].real)

                if debug_per_mode:
                    logger.info(
                            f'{tab*4}' +
                            f' L2 Norm                  : {L2: <.11e}')
                    logger.info(
                            f'{tab*4}' +
                            f' L2 Norm without left BC  : {L2_without_leftboundary: <.11e}')
                    logger.info(
                            f'{tab*4}' +
                            f' L2 Norm without right BC : {L2_without_rightboundary: <.11e}')

                    logger.info(
                            f'{tab*4}' +
                            f' L2 Norm without boundary : {L2_without_boundary: <.11e}')
 
                L2_list.append(L2) 
                L2_list_without_left.append(L2_without_leftboundary) 
                L2_list_without_right.append(L2_without_rightboundary) 
                L2_list_without_boundaries.append(L2_without_boundary)

                L_max = max(error_list[i]) 
                L_max_list.append(L_max)
                L_max_location = np.argmax(error_list[i])
                L_max_location_list.append(L_max_location)

                # L_max_location =error_list[i].index(L_max) 
                if debug_per_mode:
                    logger.info(
                            f'{tab*4}' +
                            f' L_Max                    : {L_max_list[i]: <0.11e}') 

                    logger.info(
                            f'{tab*4}' +
                            f' L_max location index     : {L_max_location_list[i] : <8}')
    
                # print(L2_list)
                # for i in range(len(dict_indicies)):
                dict_index = dict_indicies[i]
                if plot_figures_flag:
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
                
            downstream_L2.append(L2_list[1]) 
            downstream_Lmax.append(L_max_list[1]) 
            downstream_Lmax_location.append(L_max_location_list[1]) 

            downstream_L2_without_left.append(L2_list_without_left[1]) 
            downstream_L2_without_right.append(L2_list_without_right[1]) 
            downstream_L2_without_boundaries.append(L2_list_without_boundaries[1]) 

            # downstream_Lmax.append(L_max_list[1]) 

            upstream_L2.append(L2_list[0])
            upstream_Lmax.append(L_max_list[0])

            upstream_Lmax_location.append(L_max_location_list[0]) 

            upstream_L2_without_left.append(L2_list_without_left[0]) 
            upstream_L2_without_right.append(L2_list_without_right[0]) 
            upstream_L2_without_boundaries.append(L2_list_without_boundaries[0]) 

        for i in range(number_of_grids): 
            # logger.info(k_x_plus[0].real)

            # logger.info(f'{kx_downstream_list[i][0]}')
            
            kx_downstream_error.append(
            abs(kx_downstream_list[i][0] - k_x_plus[0].real))

            kx_upstream_error.append(
            abs(kx_upstream_list[i][0] - k_x_minus[0].real))
        logger.info(f"{'Convergence Study':=^90}")
        logger.info(f"{'L2-Norm Results':-^90}")
        mode_identifier_string = 'Downstream Radial Mode ' + str(i_rad_mode_num )
        logger.info(f'{mode_identifier_string :-^90}')
        logger.info(
                f"{'Gridpoints':>10}" +
                f"{'k_x_error':>17}" +
                f"{'L2':>19}" +
                f"{'L2 no left BC':>19}"+
                f"{'L2 no right BC':>19}")
        for i in range(number_of_grids): 
            logger.info(
                    f'{grid_point_array[i]:^10}'+
                    f'{kx_downstream_error[i]:<0.11e}'+
                    f'  {downstream_L2[i]: <0.11e}'+ 
                    f'  {downstream_L2_without_left[i]: <0.11e}' +
                    f'  {downstream_L2_without_right[i]: <0.11e}') 

        logger.info(
                f"{'Gridpoints':>10}" +
                f"{'L_Max':>19}" +
                f"{'L_Max Location':>19}")
        for i in range(number_of_grids): 
            logger.info(
                    f'{grid_point_array[i]:>10}'+
                    f'  {downstream_Lmax[i]: <0.11e}'+ 
                    f'  {downstream_Lmax_location[i]: >17}') 

        mode_identifier_string = 'Upstream Radial Mode ' + str(i_rad_mode_num )
        logger.info(f'{mode_identifier_string :-^90}')
        logger.info(
                f"{'Gridpoints':>10}" +
                f"{'k_x_error':>17}" +
                f"{'L2':>19}" +
                f"{'L2 no left BC':>19}"+
                f"{'L2 no right BC':>19}")
        for i in range(number_of_grids): 
            logger.info(
                    f'{grid_point_array[i]:^10}'+
                    f'{kx_upstream_error[i]:<0.11e}'+
                    f'  {upstream_L2[i]: <0.11e}'+ 
                    f'  {upstream_L2_without_left[i]: <0.11e}' +
                    f'  {upstream_L2_without_right[i]: <0.11e}') 

        logger.info(
                f"{'Gridpoints':>10}" +
                f"{'L_Max':>19}" +
                f"{'L_Max Location':>19}")
        for i in range(number_of_grids): 
            logger.info(
                    f'{grid_point_array[i]:>10}'+
                    f'  {upstream_Lmax[i]: <0.11e}'+ 
                    f'  {upstream_Lmax_location[i]: >17}') 
# for axial wavenumber ...
        # need to compute error for each wavenumber corrresponding to each radial mode

        for i in range(number_of_grids-1): 
            refinement_ratio = grid_point_array[i+1]/grid_point_array[i] # 2 for a doubling of gridpoints 
            # logger.info(kx_downstream_error[i])
            
            ROC_kx_downstream = np.log (
                    kx_downstream_error[i]/kx_downstream_error[i+1])/\
                            np.log(refinement_ratio)

            ROC_kx_upstream = np.log (
                    kx_upstream_error[i]/kx_upstream_error[i+1])/\
                            np.log(refinement_ratio)

            ROC_kx_downstream_list.append(ROC_kx_downstream)
            ROC_kx_upstream_list.append(ROC_kx_upstream)
            # logger.info(ROC_kx_downstream)
        # sys.exit()

            # ROC_kx_upstream = np.log(
            #         kx_upstream_list[i+1][0]/kx_upstream_list[i][0])/\
            # np.log( #         grid_point_array[i]/grid_point_array[i+1]) 

            # this is where i realized i kx_error was one smaller, and its because
            # I was comparing against the first grid to the second and not 
            # the first grid to the analytic solution to precision!

        # logger.info(f'{kx_downstream_error}{downstream_L2}')

# for pressure mode
        for i in range(number_of_grids-1): 
            refinement_ratio = grid_point_array[i+1]/grid_point_array[i] # 2 for a doubling of gridpoints 

            ROC_downstream = np.log(
                    downstream_L2[i]/downstream_L2[i+1])/\
                            np.log(refinement_ratio)

            ROC_downstream_without_left = np.log(
                    downstream_L2_without_left[i]/downstream_L2_without_left[i+1])/\
                            np.log(refinement_ratio)

            ROC_downstream_without_right = np.log(
                    downstream_L2_without_right[i]/downstream_L2_without_right[i+1])/\
                            np.log(refinement_ratio)

            ROC_downstream_without_boundaries = np.log(
                    downstream_L2_without_boundaries[i]/downstream_L2_without_boundaries[i+1])/\
                            np.log(refinement_ratio)

            ROC_for_L_max_downstream = np.log(
                    downstream_Lmax[i]/downstream_Lmax[i+1])/\
                            np.log(refinement_ratio) 

# puT ROC without BCs
            ROC_upstream = np.log(
                    upstream_L2[i]/upstream_L2[i+1])/\
                            np.log(refinement_ratio)

            ROC_upstream_without_left = np.log(
                    upstream_L2_without_left[i]/upstream_L2_without_left[i+1])/\
                            np.log(refinement_ratio)

            ROC_upstream_without_right = np.log(
                    upstream_L2_without_right[i]/upstream_L2_without_right[i+1])/\
                            np.log(refinement_ratio)

            ROC_upstream_without_boundaries = np.log(
                    upstream_L2_without_boundaries[i]/upstream_L2_without_boundaries[i+1])/\
                            np.log(refinement_ratio)

            ROC_for_L_max_upstream = np.log(
                    upstream_Lmax[i]/upstream_Lmax[i+1])/\
                            np.log(refinement_ratio)

            # ROC_axial_wavenumber_downstream_list.append(ROC_kx_downstream)

            ROC_downstream_list.append(ROC_downstream)
            ROC_downstream_without_left_list.append(ROC_downstream_without_left)
            ROC_downstream_without_right_list.append(ROC_downstream_without_right)
            ROC_downstream_without_boundaries_list.append(ROC_downstream_without_boundaries) 
            ROC_L_max_downstream_list.append(ROC_for_L_max_downstream)

            ROC_upstream_list.append(ROC_upstream)
            ROC_upstream_without_left_list.append(ROC_upstream_without_left)
            ROC_upstream_without_right_list.append(ROC_upstream_without_right)
            ROC_upstream_without_boundaries_list.append(ROC_upstream_without_boundaries) 
            ROC_L_max_upstream_list.append(ROC_for_L_max_upstream)

            ROC_nested_list.append(ROC_downstream_list)

        column_string=(
                f"{'Grid':^10}"+
                f"{'ROC':>14}"+
                f"{'ROC_noLBC':>14}"+
                f"{'ROC_noRBC':>14}"+
                f"{'ROC_noBCS':>14}"+
                f"{'ROC at Lmax': >14}")
        L2_downstream_DataFrame = pandas.DataFrame(
                list(zip(
                    kx_downstream_error,
                    downstream_L2,
                    downstream_L2_without_left,
                    downstream_L2_without_right,
                    downstream_L2_without_boundaries,
                    downstream_Lmax,
                    downstream_Lmax_location)),
                    columns = [
                        r'$L_{2,k_x}$',
                        r'$L_{2,\bar{p}}$',
                        r'$L_{2,noLBC}$',
                        r'$L_{2,noRBC}$',
                        r'$L_{2,noBCS}$',
                        r'$L_{max}$',
                        r'$L_{max,location}$'])
        #,index = grid_point_array)

        L2_upstream_DataFrame = pandas.DataFrame(
                list(zip(
                    kx_upstream_error,
                    upstream_L2,
                    upstream_L2_without_left,
                    upstream_L2_without_right,
                    upstream_L2_without_boundaries,
                    upstream_Lmax,
                    upstream_Lmax_location
                    )),
                columns = [
                        r'$L_{2,k_x}$',
                        r'$L_{2,\bar{p}}$',
                        r'$L_{2,noLBC}$',
                        r'$L_{2,noRBC}$',
                        r'$L_{2,noBCS}$',
                        r'$L_{max}$',
                        r'$L_{max,location}$'])#,
                        # columns = [r'\alpha',r'\alpha_{noLBC}',r'\alpha_{noRBC}',r'\alpha_{noBCS}'])#,index = grid_point_array)

        # L2_upstream_DataFrame = L2_upstream_DataFrame.style.format(precision=11).to_latex()
        ROC_downstream_DataFrame = pandas.DataFrame(
                list(zip(ROC_kx_downstream_list,
                    ROC_downstream_list,
                    ROC_downstream_without_left_list,
                    ROC_downstream_without_right_list,
                    ROC_downstream_without_boundaries_list,
                    ROC_L_max_downstream_list)),
                columns = [
                        r'$ROC_{k_x}$',
                        r'$ROC_{\bar{p}}$',
                        r'$ROC_{noLBC}$',
                        r'$ROC_{noRBC}$',
                        r'$ROC_{noBCS}$',
                        r'$ROC_{Lmax}$'])
                        # columns = [r'\alpha',r'\alpha_{noLBC}',r'\alpha_{noRBC}',r'\alpha_{noBCS}'])#,index = grid_point_array)

        ROC_upstream_DataFrame = pandas.DataFrame(
                list(zip(ROC_kx_upstream_list,
                    ROC_upstream_list,
                    ROC_upstream_without_left_list,
                    ROC_upstream_without_right_list,
                    ROC_upstream_without_boundaries_list,
                    ROC_L_max_upstream_list)),
                columns = [
                        r'$ROC_{k_x}$',
                        r'$ROC_{\bar{p}}$',
                        r'$ROC_{noLBC}$',
                        r'$ROC_{noRBC}$',
                        r'$ROC_{noBCS}$',
                        r'$ROC_{Lmax}$'])

        # fig, ax = plt.subplots()
        # plt.plot(ROC_downstream_list)
        # plt.show()
        with open('tables/L2_downstream_table.tex','w') as tf:
            tf.write(L2_downstream_DataFrame.style.format(precision=11).to_latex())
        with open('tables/L2_upstream_table.tex','w') as tf:
            tf.write(L2_upstream_DataFrame.style.format(precision=11).to_latex())
        with open('tables/ROC_downstream_table.tex','w') as tf:
            tf.write(ROC_downstream_DataFrame.style.format(precision= 11).to_latex())
        with open('tables/ROC_upstream_table.tex','w') as tf:
            tf.write(ROC_upstream_DataFrame.style.format(precision=11).to_latex())
        # pprint.pprint(L2_downstream_DataFrame)
        # sys.exit()
            
        mode_identifier_string = 'Downstream Radial Mode ' + str(i_rad_mode_num )
        logger.info(f'{mode_identifier_string :-^90}')
        logger.info(
                f"{'Grid':^10}"+
                f"{'ROC_kx':>13}"+
                f"{'ROC':>14}"+
                f"{'ROC_noLBC':>14}"+
                f"{'ROC_noRBC':>14}"+
                f"{'ROC_noBCS':>14}"+
                f"{'ROC at Lmax': >14}")

        for i in range(number_of_grids-1): 
            logger.info(
                    f'{i : ^10}' +
                    f'{ROC_kx_downstream_list[i]:.11f}'+
                    f' {ROC_downstream_list[i]:.11f}'+
                    f' {ROC_downstream_without_left_list[i]:.11f}'+
                    f' {ROC_downstream_without_right_list[i]:.11f}'+
                    f' {ROC_downstream_without_boundaries_list[i]:.11f}'+
                    f' {ROC_L_max_downstream_list[i]:.11f}')

        mode_identifier_string = 'Upstream Radial Mode ' + str(i_rad_mode_num )
        logger.info(f'{mode_identifier_string :-^90}')
        logger.info(
                f"{'Grid':^10}"+
                f"{'ROC_kx':>13}"+
                f"{'ROC':>14}"+
                f"{'ROC_noLBC':>14}"+
                f"{'ROC_noRBC':>14}"+
                f"{'ROC_noBCS':>14}"+
                f"{'ROC at Lmax': >14}")

        for i in range(number_of_grids-1): 
            logger.info(
                    f'{i : ^10}' +
                    f'{ROC_kx_upstream_list[i]:.11f}'+
                    f' {ROC_upstream_list[i]:.11f}'+
                    f' {ROC_upstream_without_left_list[i]:.11f}'+
                    f' {ROC_upstream_without_right_list[i]:.11f}'+
                    f' {ROC_upstream_without_boundaries_list[i]:.11f}'+
                    f' {ROC_L_max_upstream_list[i]:.11f}')


            # logger.info(f'{i : ^10}' +
            #         f'{ROC_upstream_list[i]:.12} {ROC_L_max_upstream_list[i]:.12}')
            # for axial wavenumber
            # logger.info(
                    # f'{ROC_axial_wavenumber_downstream_list[i]:.12}')




if __name__ == '__main__':
    main()
