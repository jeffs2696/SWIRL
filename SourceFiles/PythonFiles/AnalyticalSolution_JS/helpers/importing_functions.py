#!/usr/bin/env python3
import pandas

def importSwirlOutput(grid_point_array):
    second_order_directories = []
    fourth_order_directories = [] 
    filename_list = []
    NumericalAxialWavenumberData_second_order_list = []
    NumericalAxialWavenumberData_fourth_order_list = []

    for i_gp,j_gp in enumerate(grid_point_array):
        grid_iteration_string = 'Grid Iteration ' + str(i_gp)
        grid_point_string = '# of Gridpoints ' + str(j_gp)
        # Grid Loop

        print(f'{grid_iteration_string: ^20}')
        print(f'{grid_point_string: ^20}')

        
        # logging.info('Iteration       ' + str(i))
        # logging.info('# of Gridpoints ' + str(j))
    
        # print('Importing files for this grid...')

        second_order_directories.append(
                '../../../CodeRun/03-EVanalysis/SWIRLVerification/' + 
                'UniformFlowCylinderHardwall/SecondOrderDiff/{n}pts/'.format(
                    n=str(grid_point_array[i_gp])))

        fourth_order_directories.append(
                '../../../CodeRun/03-EVanalysis/SWIRLVerification/' + 
                'UniformFlowCylinderHardwall/FourthOrderDiff/{n}pts/'.format(
                    n=str(grid_point_array[i_gp])))

        if i_gp <= 1:
            filename_list.append(
                    'Test1_npts{n}_fd1_domain_cgam.nonconv_acc.00{n}'.format(
                        n=str(grid_point_array[i_gp]))) 

        else: 
            filename_list.append(
                    'Test1_npts{n}_fd1_domain_cgam.nonconv_acc.0{n}'.format(
                        n=str(grid_point_array[i_gp])))

        NumericalAxialWavenumberData_fourth_order = \
                pandas.read_csv(
                        fourth_order_directories[i_gp] +
                        filename_list[i_gp], delim_whitespace = True )

        NumericalAxialWavenumberData_fourth_order_list.append(
                NumericalAxialWavenumberData_fourth_order)

        NumericalAxialWavenumberData_second_order = \
                pandas.read_csv(
                        second_order_directories[i_gp] +
                        filename_list[i_gp], delim_whitespace = True )

        NumericalAxialWavenumberData_second_order_list.append(
                NumericalAxialWavenumberData_second_order)
    return second_order_directories,\
            fourth_order_directories,\
            NumericalAxialWavenumberData_second_order_list,\
            NumericalAxialWavenumberData_fourth_order_list
