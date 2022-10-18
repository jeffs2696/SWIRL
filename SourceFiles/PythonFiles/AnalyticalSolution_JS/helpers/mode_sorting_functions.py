#!/usr/bin/env python3
import sys
import helpers.helper_functions as fcn 

def sortAxialWavenumbers(
        axial_mach_number,
        wavenumber,
        num_of_zeros,
        NumericalAxialWavenumberData_list,
        grid_point_array):

    """ 
    Objective
    ---------
    Create a dictionary data type that contains the sorted axial wavenumbers and
    radial mode data from Swirl. Currently SWIRL outputs the raw data only sorted
    by zero crossing i.e. radial mode number. This code is a prototype for 
    how Swirl will limit the number of modes saved to file.

    This code sorts axial wavenumbers with an "allowable decay rate" (See docs 
    for more information). Once the axial wavenumbers within a certain range of
    values has been chosen, their corresponding radial modes are imported and
    appended to the dictionary


    Parameters
    ----------
    NumericalAxialWavenumberData_list : nested list



    Returns
    -------
    mode_dictionary : dict

    """

    mode_dictionary = {
            'k_x' : None,
            'radial_mode_number': None, 
            'radial_mode_index': None ,
            'radial_mode_data' : None
            }

    # print(len(NumericalAxialWavenumberData_list))
    # sys.exit()
    
    if axial_mach_number > 0:
        k_x_convective = wavenumber/axial_mach_number
        max_real_part = 33 #k_x_convective
        max_imag_part = 14
        min_imag_part = -14
    else: 
        max_real_part = 33
        max_imag_part = 14
        min_imag_part = -14
   
    for index,row in NumericalAxialWavenumberData_list.iterrows(): 

        if NumericalAxialWavenumberData_list['Re{gam}'][index] == 0.0 and \
                NumericalAxialWavenumberData_list['Im{gam}'][index] == 0.0:
                    # print('k_x = 0 , 0j, trivial, omitted')
                    pass

        elif NumericalAxialWavenumberData_list['Re{gam}'][index] < max_real_part and \
                NumericalAxialWavenumberData_list['Im{gam}'][index] < max_imag_part and \
                NumericalAxialWavenumberData_list['Im{gam}'][index] > min_imag_part and \
                NumericalAxialWavenumberData_list['nz'][index] < num_of_zeros:

                if mode_dictionary['radial_mode_index'] == None:
                    # if there isn't a value for the radial mode index, then
                    # this is the first iteration on a new dictionary ..  
                    mode_dictionary.update({
                        'k_x': (
                            NumericalAxialWavenumberData_list['Re{gam}'][index],
                            NumericalAxialWavenumberData_list['Im{gam}'][index]),
                            'radial_mode_number': NumericalAxialWavenumberData_list['nz'][index],
                            'radial_mode_index': NumericalAxialWavenumberData_list['#'][index],
                            }) 

                else:
                    fcn.append_value(mode_dictionary, 'k_x', (NumericalAxialWavenumberData_list['Re{gam}'][index], NumericalAxialWavenumberData_list['Im{gam}'][index]))
                    fcn.append_value(mode_dictionary, 'radial_mode_number', NumericalAxialWavenumberData_list['nz'][index])
                    fcn.append_value(mode_dictionary, 'radial_mode_index',  NumericalAxialWavenumberData_list['#'][index])

    for i,j in enumerate(mode_dictionary['radial_mode_index']):
        # index_grid_string = 'egv_np_00' + 
        # print((grid_point_array))
        pass
        
    
    return mode_dictionary
#                 plt.scatter(
#                         filtered_wavenumber_real,
#                         filtered_wavenumber_imag,
#                         marker = 'd',# changes triangle direction
#                         # label = ii,
#                         facecolor ='none', edgecolors ='b',
#                         )
    
#                 if NumericalAxialWavenumberData_list[ii]['Im{gam}'][index] > 0 or \
#                         NumericalAxialWavenumberData_list[ii]['Im{gam}'][index] < 0: 
    
#                     ax.annotate(r'$K_{{{M},{N}}}$'.format(M = azimuthal_mode_number,N = NumericalAxialWavenumberData_list[ii]['nz'][index]),
#                             xy=(
#                                 NumericalAxialWavenumberData_list[ii]['Re{gam}'][index],
#                                 NumericalAxialWavenumberData_list[ii]['Im{gam}'][index]
#                                 ),
#                             xycoords='data',
#                             textcoords='offset points',
#                             xytext = (-20,-20),
#                             horizontalalignment='center',
#                             verticalalignment='bottom',
#                             fontsize='10',
#                             arrowprops=dict(arrowstyle= '-',
#                                 color='blue',
#                                 lw=1,
#                                 ls='--')
#                             )
    
#                 else:
#                     ax.annotate(r'$K_{{{M},{N}}}$'.format(M = azimuthal_mode_number,N = NumericalAxialWavenumberData_list[ii]['nz'][index]),
#                             xy=(
#                                 NumericalAxialWavenumberData_list[ii]['Re{gam}'][index],
#                                 NumericalAxialWavenumberData_list[ii]['Im{gam}'][index]),
#                             xycoords='data',
#                             textcoords='offset points',
#                             xytext = (0,-20),
#                             horizontalalignment='center',
#                             verticalalignment='center',
#                             fontsize='10',
#                             arrowprops=dict(arrowstyle= '-',
#                                 color='blue',
#                                 lw=1,
#                                 ls='--')
#                             )
                    
#                     index_for_mode_import.append(NumericalAxialWavenumberData_list[ii]['#'][index]) 
#                     # print(ii) 
