#!/usr/bin/env python3
import pandas

def importNumericalWavenumbers(grid_point_array):
    """ 
    Return Swirl's axial wavenumber outputs 

    Parameters
    ----------
    grid_point_array : np.array
        An array of gridpoints which each have a directory within a finite 
        difference directory

    Returns
    -------
    second_order_directories : list
        list of the second order directories for each grid

    fourth_order_directories : list
        a list of the fourth order directories for each grid

    NumericalAxialWavenumberData_second_order_list: list
        a list of pandas Dataframes which contain all of the wavenumbers found
        in Swirl using a second order central finite difference method for 
        the radial derivatives in the Linearized Euler Equations 

    NumericalAxialWavenumberData_fourth_order_list: list
        a list of pandas Dataframes which contain all of the wavenumbers found
        in Swirl using a fourth order central finite difference method for 
        the radial derivatives in the Linearized Euler Equations

    """
    second_order_directories = []
    fourth_order_directories = [] 
    second_order_filename_list = []
    fourth_order_filename_list = []
    NumericalAxialWavenumberData_second_order_list = []
    NumericalAxialWavenumberData_fourth_order_list = []

    for i_gp,j_gp in enumerate(grid_point_array):
        grid_iteration_string = 'Grid Iteration ' + str(i_gp)
        grid_point_string = '# of Gridpoints ' + str(j_gp)
        # Grid Loop

        # print(f'{grid_iteration_string: ^20}')
        # print(f'{grid_point_string: ^20}')
        
        second_order_directories.append(
                '../../../CodeRun/03-EVanalysis/SWIRLVerification/' + 
                'UniformFlowCylinderHardwall/SecondOrderDiff/{n}pts/'.format(
                    n=str(grid_point_array[i_gp])))

        fourth_order_directories.append(
                '../../../CodeRun/03-EVanalysis/SWIRLVerification/' + 
                'UniformFlowCylinderHardwall/FourthOrderDiff/{n}pts/'.format(
                    n=str(grid_point_array[i_gp])))

        # print(i_gp,j_gp)

        # grid points that are on file names have 0's as place holders so
        # to account for this, check if the # of points is larger than 100 
        if j_gp < 100:

            second_order_filename_list.append(
                    'Test1_npts{n}_fd1_domain_cgam.nonconv_acc.00{n}'.format(
                        n=str(grid_point_array[i_gp]))) 
                    #needs to be fixed for fd2 as well

            fourth_order_filename_list.append(
                    'Test1_npts{n}_fd2_domain_cgam.nonconv_acc.00{n}'.format(
                        n=str(grid_point_array[i_gp]))) 
                    #needs to be fixed for fd2 as well

        elif j_gp <1000: 

            second_order_filename_list.append(
                    'Test1_npts{n}_fd1_domain_cgam.nonconv_acc.0{n}'.format(
                        n=str(grid_point_array[i_gp])))

            fourth_order_filename_list.append(
                    'Test1_npts{n}_fd2_domain_cgam.nonconv_acc.0{n}'.format(
                        n=str(grid_point_array[i_gp]))) 
        else:
            second_order_filename_list.append(
                    'Test1_npts{n}_fd1_domain_cgam.nonconv_acc.{n}'.format(
                        n=str(grid_point_array[i_gp])))

            fourth_order_filename_list.append(
                    'Test1_npts{n}_fd2_domain_cgam.nonconv_acc.{n}'.format(
                        n=str(grid_point_array[i_gp]))) 

        NumericalAxialWavenumberData_fourth_order = \
                pandas.read_csv(
                        fourth_order_directories[i_gp] +
                        fourth_order_filename_list[i_gp], delim_whitespace = True )

        NumericalAxialWavenumberData_fourth_order_list.append(
                NumericalAxialWavenumberData_fourth_order)

        NumericalAxialWavenumberData_second_order = \
                pandas.read_csv(
                        second_order_directories[i_gp] +
                        second_order_filename_list[i_gp], delim_whitespace = True )

        NumericalAxialWavenumberData_second_order_list.append(
                NumericalAxialWavenumberData_second_order)

    return second_order_directories,\
            fourth_order_directories,\
            NumericalAxialWavenumberData_second_order_list,\
            NumericalAxialWavenumberData_fourth_order_list


    return
