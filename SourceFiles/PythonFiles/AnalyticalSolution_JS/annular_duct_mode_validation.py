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

pandas.options.display.float_format = '{:.12E}'.format
# get logging settings 
logger = lfcn.log_to_console()

# used to print indents to log
tab = '--'  

def main(): 
    
    logger.info(
            f"{'Validation Study for Uniform Flow in Annular Ducts':=^90}")

    debug = True
    debug_per_mode = True
    plot_figures_flag = False

    
    fcn.set_plot_parameters() #plot parameters
    plt.rcParams['axes.grid'] = True

    # test case parameters
    
    
    num_of_zeros = 1  # each zero corresponds to radial modes
    wavenumber = 10 
    azimuthal_mode_number = 2 
    axial_mach_number = 0.3
    r_min = 0.5
    r_max = 1
    #fd2 
    grid_point_array = np.array([121,161, 201, 241 ,281,321, 361, 401, 441, 481, 521,561])
    # grid_point_array = np.array([121,161, 201, 241, 281,321, 361, 401, 441, 481, 521, 561, 601, 641, 681, 721, 761, 801, 841 ])#, 132, 264,528,1056])
    #fd1
    # grid_point_array = np.array([ 261,301, 341, 381 , 421, 461, 501,541,581,621,661,701,741,781])#, 132, 264,528,1056])

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
        
    numeric_mode_list = []
    analytic_mode_list = []
    error_list = []
    wavenumber_error_list = []
    L2_list = []
    L_max_list = []
    L_max_location_list = []
    ROC_list = []
    ROC_L_max_list = []
    ROC_kx_list = []

    analytic_mode_data_filepath = \
            f'../../../CodeRun/03-EVanalysis/AnnularDuctModeVerificationTestCase/'
    directories = \
            f'../../../CodeRun/03-EVanalysis/AnnularDuctModeVerificationTestCase/'
    filename_list = []
    NumericalAxialWavenumberData_list = []
    analytical_mode_data_list = []

    analytical_mode_parameter_data_list = []
    numerical_mode_data_even_list = []
    numerical_mode_data_odd_list = []
    numerical_wavenumber_list = []
    analytic_wavenumber_list = []

    radius_list = []
    dr_list = []

    indexCounter = 1
    for i_num_of_radial_modes in range(num_of_zeros):

        for i_gp,j_gp in enumerate(grid_point_array): 
            grid_iteration_string = 'Grid Iteration ' + str(i_gp)
            grid_point_string = '# of Gridpoints ' + str(j_gp)
            # Grid Loop
    
            logger.info(f'{grid_iteration_string: ^20}')
            logger.info(f'{grid_point_string: ^20}')
            
            # print(i_gp,j_gp)
    
            # grid points that are on file names have 0's as place holders so
            # to account for this, check if the # of points is larger than 100 
            if j_gp < 100:
                
    
                filename_list.append(
                        'Test1_npts{n}_fd2_domain_a_gam.nonconv_acc.00{n}'.format(
                            n=str(grid_point_array[i_gp]))) 
                        #needs to be fixed for fd2 as well

                #filename_list.append(
                #        'Test1_npts{n}_fd1_domain_a_gam.nonconv_acc.00{n}'.format(
                #            n=str(grid_point_array[i_gp]))) 
                #        #needs to be fixed for fd2 as well
    
            elif j_gp <1000: 
                filename_list.append(
                        'Test1_npts{n}_fd2_domain_a_gam.nonconv_acc.0{n}'.format(
                            n=str(grid_point_array[i_gp]))) 
                # filename_list.append(
                #         'Test1_npts{n}_fd1_domain_a_gam.nonconv_acc.0{n}'.format(
                #             n=str(grid_point_array[i_gp]))) 
            else:
                filename_list.append(
                        'Test1_npts{n}_fd2_domain_a_gam.nonconv_acc.{n}'.format(
                            n=str(grid_point_array[i_gp])))
                # filename_list.append(
                #         'Test1_npts{n}_fd1_domain_a_gam.nonconv_acc.{n}'.format(
                #             n=str(grid_point_array[i_gp])))
    
            NumericalAxialWavenumberData = \
                    pandas.read_csv(
                            directories +
                           filename_list[i_gp], delim_whitespace = True )
            if (debug):
                pprint.pprint(NumericalAxialWavenumberData)
    
            NumericalAxialWavenumberData_list.append(
                    NumericalAxialWavenumberData)
    
    
    
            # i_num_of_radial_modes = 0
            analytic_mode_parameter_filename = \
                    f'radial_mode_parameters0{i_num_of_radial_modes+1}_np_0{j_gp}.dat'.format() 
            analytic_mode_filename = \
                    f'analytic_radial_mode_data_radial_mode_number_0{i_num_of_radial_modes+1}_np_0{j_gp}.dat'.format() 
    
            analytic_mode_data = pandas.read_csv(
                    analytic_mode_data_filepath + analytic_mode_filename ,
                    delim_whitespace = True) 

            radius_list.append(analytic_mode_data['radius'])
            dr_list.append(analytic_mode_data['radius'][1] - analytic_mode_data['radius'][0])
            analytical_mode_data_list.append(analytic_mode_data)

            analytic_mode_parameter_data = pandas.read_csv(
                    '../../../CodeRun/' + analytic_mode_parameter_filename ,
                    delim_whitespace = True) 

            analytical_mode_parameter_data_list.append(analytic_mode_parameter_data)
    
            numeric_mode_filename_odd = \
                    f'Test1_npts{j_gp}_fd2_domain_a_numerical_mode_shapes_upstream_cuton_radial_mode_number_0{i_num_of_radial_modes+1}_np_0{j_gp}'.format()
        
    
            numeric_mode_filename_even = \
                    f'Test1_npts{j_gp}_fd2_domain_a_numerical_mode_shapes_downstream_cuton_radial_mode_number_0{i_num_of_radial_modes+1}_np_0{j_gp}'.format()

            # numeric_mode_filename_odd = \
            #         f'Test1_npts{j_gp}_fd1_domain_a_numerical_mode_shapes_upstream_radial_mode_number_0{i_num_of_radial_modes+1}_np_0{j_gp}'.format()
        
    
            # numeric_mode_filename_even = \
            #         f'Test1_npts{j_gp}_fd1_domain_a_numerical_mode_shapes_downstream_radial_mode_number_0{i_num_of_radial_modes+1}_np_0{j_gp}'.format()
    
            numeric_mode_data_odd = pandas.read_csv(
                    analytic_mode_data_filepath + numeric_mode_filename_odd ,
                    delim_whitespace = True) 

            numeric_mode_data_even = pandas.read_csv(
                    analytic_mode_data_filepath + numeric_mode_filename_even , 
                    delim_whitespace = True) 
    
            numerical_mode_data_even_list.append(numeric_mode_data_even)

            numerical_mode_data_odd_list.append(numeric_mode_data_odd)
    
            if (debug):
                print(analytic_mode_filename)
                print(numeric_mode_filename_odd)
                print(numeric_mode_filename_even)
            # pprint.pprint(numerical_mode_data_odd_list)
            # pprint.pprint(numerical_mode_data_even_list)
    
    
    
            # if (j_gp < 322): 
                # numeric_mode_data = numerical_mode_data_even_list[i_gp]
            # else:
                # numeric_mode_data = numerical_mode_data_odd_list[i_gp]



            numeric_mode_data = numerical_mode_data_odd_list[i_gp]

            if (sum(numeric_mode_data['real'])<0): 
                numeric_mode_data['real'] = -numeric_mode_data['real']
            else: 
                pass

            
            analytic_mode_data =analytical_mode_data_list[i_gp] 



            analytic_wavenumber= (afcn.get_axial_wavenumbers(
                    azimuthal_mode_number,
                    wavenumber, 
                    axial_mach_number,
                    analytic_mode_parameter_data['non_dimensional_roots']))

            analytic_wavenumber_list.append(analytic_wavenumber[1][0])

            ## Computing analytic wavenumber
            pprint.pprint(analytic_mode_parameter_data)

            # sys.exit()
            ## Normalizing Mode Data
        
            numeric_normalization_factor = afcn.normalize_psi(numeric_mode_data['real'],numeric_mode_data['radius'])
    
            # sys.exit()
            # if ((i_num_of_radial_modes+1)%2 == 1):
            #     # if the radial mode number is odd
            #     numeric_normalization_factor = -numeric_normalization_factor
            # else: 
            #     numeric_normalization_factor = numeric_normalization_factor

            numeric_mode = numeric_normalization_factor*numeric_mode_data['real']
                
    
            # if (j_gp >300):
            #     numeric_normalization_factor = -numeric_normalization_factor

            numeric_mode_list.append(numeric_mode)
            
        
            analytic_normalization_factor = afcn.normalize_psi(analytic_mode_data['pressure'].astype(float),analytic_mode_data['radius'])
        
            # if ((i_num_of_radial_modes+1)%2 == 1):
            #     # if the radial mode number is odd
            #     analytic_normalization_factor = -analytic_normalization_factor
            # else:
            #     analytic_normalization_factor = analytic_normalization_factor
    
            # if (j_gp <200):
            #         analytic_normalization_factor = -analytic_normalization_factor
    
            analytic_mode = analytic_normalization_factor*analytic_mode_data['pressure'] 
    
            analytic_mode_list.append(analytic_mode)
    
            # error calc


# only true for cut on !
            numerical_wavenumber = numeric_mode_data['kx_real'][0]#NumericalAxialWavenumberData_list[i_gp]['Re{gam}'][0] 


            # sys.exit()
            # if (j_gp <322):
            #                 # else: 
            #     numerical_wavenumber =NumericalAxialWavenumberData_list[i_gp]['Re{gam}'][1] 
                
            numerical_wavenumber_list.append(numerical_wavenumber)
        
            error = abs((analytic_mode) - (numeric_mode)) 

            print(analytic_wavenumber)
            
            
            print(numerical_wavenumber)

            wavenumber_error = abs(abs(numerical_wavenumber_list[i_gp])-abs(analytic_wavenumber_list[i_gp])) 
            # fig = plt.figure()
            
            # plt.plot(
            #         radius_list[i_gp],
            #         analytic_mode_list[i_gp],
            #         label = 'Analytic')
    
            # plt.plot(
            #         radius_list[i_gp],
            #         numeric_mode_list[i_gp],
            #         linestyle='dashed',
            #         label = 'Numerical')
            # plt.ylabel('Pressure[-]')
            # plt.xlabel('Radius [-]')
            # plt.legend() 
                
            # plt.savefig( 
            #         fname      ='figures/fourth_order_annular_mode_shape_comparions_rmn1_{n}.pdf'.format(
            #                         n = str(grid_point_array[i_gp])),
            #                     format     ='pdf')
            # fig = plt.figure()
            # plt.plot(
            #         analytic_mode_data['radius'],
            #         error) 
            # plt.savefig( 
            #         fname      ='figures/annular_mode_shape_comparions_rmn1_error_{n}.pdf'.format(
            #                         n = str(grid_point_array[i_gp])),
            #                     format     ='pdf')
            # plt.show()

            L2 = mfcn.getL2norm(
                    (analytic_mode_list[i_gp]),
                    (numeric_mode_list[i_gp]))
    
            L_max = max(error)
            L_max_location = np.argmax(error)
    
            logger.info(
                    f'{tab}' +
                    f' L2 Norm                  : {L2: <.11e}')
            logger.info(
                    f'{tab}' +
                    f' L_max                    : {L_max: <.11e}')
            logger.info(
                    f'{tab}' +
                    f' L_max_location index     : {L_max_location: <8}')
    
            wavenumber_error_list.append(wavenumber_error)
            error_list.append(error)
            L2_list.append(L2)
            L_max_list.append(L_max)
            L_max_location_list.append(L_max_location)
    
            analytic_mode_filename_str = f'analytical_mode_data_{j_gp}.dat'.format()
            numeric_mode_filename_str = f'numerical_mode_data_{j_gp}.dat'.format()
            error_filename_str = f'error_output_{j_gp}.dat'.format()
            with open(error_filename_str, "w") as file:
                for item1,item2 in zip(radius_list[:][i_gp],error_list[:][i_gp]):
                    file.write("{}\t{}\n".format(str(item1),str(item2)))
            with open(numeric_mode_filename_str, "w") as file:
                for item1,item2 in zip(radius_list[:][i_gp],numeric_mode_list[:][i_gp]):
                    file.write("{}\t{}\n".format(str(item1),str(item2)))
            with open(analytic_mode_filename_str, "w") as file:
                for item1,item2 in zip(radius_list[:][i_gp],analytic_mode_list[:][i_gp]):
                    file.write("{}\t{}\n".format(str(item1),str(item2)))
            # pprint.pprint(analytic_mode_data['radius'])
            logger.info(wavenumber_error)
    
        for i in range(number_of_grids-1): 
            refinement_ratio = grid_point_array[i+1]/grid_point_array[i] # 2 for a doubling of gridpoints 
            ROC = np.log(
                    L2_list[i]/L2_list[i+1])/\
                            np.log(refinement_ratio) 
            ROC_L_max = np.log(
                    L_max_list[i]/L_max_list[i+1])/\
                            np.log(refinement_ratio) 

            ROC_kx = np.log(
                    wavenumber_error_list[i]/wavenumber_error_list[i+1])/\
                            np.log(refinement_ratio) 
    
            ROC_list.append(ROC)

            ROC_L_max_list.append(ROC_L_max)

            ROC_kx_list.append(ROC_kx)

            logger.info(
                    f"{i : ^10}" +
                    f"{ROC_list[i] :.11f}")

            logger.info(
                    f"{i : ^10}" +
                    f'{ROC_L_max :.11f}')


            logger.info(
                    f"{i : ^10}" +
                    f"{ROC_kx :.11f}")

        # plot data 
        # for i_gp,j_gp in enumerate(grid_point_array):
        #     fig = plt.figure()
        #     plt.plot(
        #             # analytic_mode_data['radius'],
        #             analytic_mode_list[i_gp],
        #             label = 'Analytic')
    
        #     plt.plot(
        #             # numeric_mode_data['radius'],
        #             numeric_mode_list[i_gp],
        #             linestyle='dashed',
        #             label = 'Numerical')
        #     plt.ylabel('Pressure[-]')
        #     plt.xlabel('Radius [-]')
        #     plt.legend() 
                
        #     plt.savefig( 
        #             fname      ='figures/annular_mode_shape_comparions_rmn1_{n}.pdf'.format(
        #                             n = str(grid_point_array[i_gp])),
        #                         format     ='pdf')
        #     fig = plt.figure()
        #     plt.plot(analytic_mode_data['radius'], error) 
        #     plt.show()
        #     plt.savefig( 
        #             fname      ='figures/annular_mode_shape_comparions_rmn1_error_{n}.pdf'.format(
        #                             n = str(grid_point_array[i_gp])),
        #                         format     ='pdf')
        
        
        
        # print(radius_list)
        # sys.exit()

        # print(error_list[:][1])
        # sys.exit()
        error_DataFrame = pandas.DataFrame(
                list(zip(error_list[:])))
        L2_DataFrame = pandas.DataFrame(
                list(zip(
                    wavenumber_error_list[:],
                    L2_list,
                    L_max_list,
                    L_max_location_list)),
                    columns = [
                        r'$L_{2,{k_x}}$',
                        r'$L_{2,\bar{p}}$',
                        r'$L_{max}$',
                        r'$L_{max,location}$'])
        ROC_DataFrame = pandas.DataFrame(
                list(zip(
                    ROC_kx_list,
                    ROC_list,
                    ROC_L_max_list,
                    )),
                    columns = [
                        r'$ROC_{L{2,{k_x}}}$',
                        r'$ROC_{L{2,\bar{p}}}$',
                        r'$ROC_{L_{max}}$',
                        ])

        format_dict = {col_name: '{:.6g}' for col_name in L2_DataFrame.select_dtypes(float).columns}


        # L2_DataFrame_table = L2_DataFrame.style.format(format_dict)
        L2_DataFrame_table = L2_DataFrame.style.hide(axis="index").format(format_dict).to_latex(hrules=True)

        format_dict = {col_name: '{:.6g}' for col_name in ROC_DataFrame.select_dtypes(float).columns}

        # ROC_DataFrame_table = ROC_DataFrame.style.format().to_latex()
        ROC_DataFrame_table = ROC_DataFrame.style.hide(axis="index").format(format_dict).to_latex(hrules=True)
        pprint.pprint(L2_DataFrame_table)
        pprint.pprint(ROC_DataFrame_table)
        
        fig, ax = plt.subplots(
            nrows=1,
            ncols=1,
            sharex=True)#, figsize=set_size(width),)

        ax.loglog(
                grid_point_array,
                wavenumber_error_list,
                marker = '+',
                markersize = 10,
                label = 'Approximate kx')
        ax.loglog(
                grid_point_array,
                L_max_list,
                marker = '+',
                markersize = 10,
                label = 'Approximate Lmax')

        ax.loglog(
                grid_point_array,
                L2_list,
                marker = '+',
                markersize = 10,
                label = 'Approximate')
        x = grid_point_array
        y = L2_list
        b = L2_list[-1]
        k = 4
        y_expected = 10**(-np.log10(x**k) + np.log10(x[-1]**k * b))
        plt.plot(
                x,
                y_expected, 
                label = 'Expected')

        ax.set_ylabel(r'$L_2$') 
        ax.set_xlabel(r'Number of Grid Points') 
        ax.legend()
        plt.savefig('figures/fourth_order_L2_annular_test.pdf',
                format='pdf',
                bbox_inches='tight')


        fig, ax = plt.subplots(
            nrows=1,
            ncols=1,
            sharex=True)
        ax.semilogx(
                dr_list[1:],
                ROC_kx_list,
                marker = '+',
                markersize = 10,
                )
        ax.semilogx(
                dr_list[1:],
                ROC_L_max_list,
                marker = '+',
                markersize = 10,
                )
        ax.semilogx(
                dr_list[1:],
                ROC_list,
                marker = '+',
                markersize = 10,
                )
        ax.set_ylabel(r'Rate of Convergence, $\alpha$') 
        ax.set_xlabel(r'$\Delta r$')
        

        
        plt.savefig('figures/fourth_order_ROC_annular_test.pdf',
                format='pdf',
                bbox_inches='tight')

        
        
        plt.show()
        with open('tables/fourth_order_L2_table.tex','w') as tf:
            tf.write(L2_DataFrame_table)
        with open('tables/fourth_order_ROC_table.tex','w') as tf:
            tf.write(ROC_DataFrame_table)

        #,index = grid_point_array)
        pprint.pprint(error_DataFrame)
        pprint.pprint(L2_DataFrame)
        pprint.pprint(ROC_DataFrame)
        # plt.show()
if __name__ == '__main__':
    main()
