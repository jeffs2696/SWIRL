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
import helpers.analytic_functions as afcn
import helpers.plotting_functions as pfcn 
import helpers.helper_functions as fcn
import matplotlib as mpl
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D # needed for 3d plots
from matplotlib import cm

#plot parameters 
fcn.set_plot_parameters()
width = 500 # to match page width in TeX doc

# test case parameters

num_of_zeros = 5 # each zero corresponds to radial modes
# no flow
wavenumber = 10 

azimuthal_mode_number = 2 

axial_mach_number = 0.3
radial_mode_number = 1

DuctModeObject = DuctModeClass(
        num_of_zeros,
        wavenumber,
        radial_mode_number,
        azimuthal_mode_number,
        axial_mach_number)


# Desired Decay Rate 
if axial_mach_number > 0:
    k_x_convective = wavenumber/axial_mach_number
    max_real_part = 33 #k_x_convective
    max_imag_part = 14
    min_imag_part = -14
else: 
    max_real_part = 33
    max_imag_part = 14
    min_imag_part = -14

# ------------


r_min = 0
r_max = 1

# Currently this code obtains the analytical solution for a uniform flow 
# cylinder

# [1] importing results from SWIRL
grid_point_array = np.array([32, 64, 128, 256])

second_order_directories = []
fourth_order_directories = [] 
filename_list = []
NumericalAxialWavenumberData_list = []
 
index_for_mode_import = []
key_list_for_mode_dictionary =[ "radial_mode", "index"] 

mode_dictionary = {
        'k_x' : None,
        'radial_mode_number': None, 
        'radial_mode_index': None ,
        'radial_mode_data': None
        }

mode_nested_dictionary = {}

# for i_rad_mode_num in range(num_of_zeros):
for i_rad_mode_num in range(0,1):
    radial_mode_iteration_string = 'Radial Mode Number ' + str(i_rad_mode_num)
    list_list = []
# for i_rad_mode_num in range(num_of_zeros):

    
    # radial mode number loop

    radial_mode_number = i_rad_mode_num
    # list_list = []

    print(f'{radial_mode_iteration_string: ^20}')
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
                    'gam.nonconv_acc.00{n}'.format(
                        n=str(grid_point_array[i_gp]))) 

        else: 
            filename_list.append(
                    'gam.nonconv_acc.0{n}'.format(
                        n=str(grid_point_array[i_gp])))

        NumericalAxialWavenumberData_fourth_order = \
                pandas.read_csv(  fourth_order_directories[i_gp] + filename_list[i_gp], delim_whitespace = True )

        # NumericalAxialWavenumberData_second_order = \
                # pandas.read_csv(  second_order_directories[i_gp] + filename_list[i_gp], delim_whitespace = True )

        NumericalAxialWavenumberData_list.append(NumericalAxialWavenumberData_fourth_order)
        # NumericalAxialWavenumberData_list.append(NumericalAxialWavenumberData_second_order)
    
        # Done importing files 
        
        # Getting analytical radial mode shapes and axial wavenumbers  

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
        # This depend on whether you count the first 0 at the boundary or not (?)

        # "allocating" lists for radial mode data for each grid
        
        normalized_radial_mode_list = []
        analytical_normalized_radial_mode_list = []
        
        
        # normalizing radial modes in a loop
        for i,j in enumerate(radial_mode_list):
        
            radial_mode = radial_mode_list[i]
            radial_mode_number_for_list = i
            normalization_constant = afcn.normalize_psi((radial_mode),r)
            if (radial_mode_number%2) == 0:
                normalized_radial_mode = normalization_constant*radial_mode
            else: 
                normalized_radial_mode = -normalization_constant*radial_mode
    
    
            normalized_radial_mode_list.append(normalized_radial_mode) 
        
            if azimuthal_mode_number == 0 and radial_mode_number_for_list == 0:
                #from Fund. Of Duct Acs - Rienstra 
                analytical_normalization_constant = np.sqrt(2)     
            else: 
                analytical_normalization_constant = np.sqrt(2)/(
                        (
                            (scp.special.jv(
                                azimuthal_mode_number,
                                Jv_p_zero[radial_mode_number_for_list]))
                            )*np.sqrt(
                                1 - azimuthal_mode_number**2/
                                Jv_p_zero[radial_mode_number_for_list]**2
                                )
                            )
                analytical_normalized_radial_mode = \
                        analytical_normalization_constant*(radial_mode) 
        
                analytical_normalized_radial_mode_list.append(
                        analytical_normalized_radial_mode
                    )
        
    
    # plotting data 
    # -----------------------------------------------------------------------------
    # Data set 1 - Analytical Wavenumbers 
        fig,ax = plt.subplots(
                constrained_layout=True)#, figsize=fcn.set_size(width))
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
    
    # plt.show()
        
    #------------------------------------------------------------------------------
    # for i in range(len(fourth_order_directories)):
        mode_nested_dictionary[i_gp] = {i_gp: mode_dictionary} 
    
        # print(mode_nested_dictionary)
    
    
    # TODO: make the loop through different grids work..
    # Issue: Radial modes that correspond to the axial wavenumbers are not appearing 
    # due to some issue in the normalization (?) 
    # for ii in range(len(grid_point_array)):#range(0,1): # this loops through different grids
    
        # Needed to reset the current looP
        mode_dictionary = {
                'k_x' : None,
                'radial_mode_number': None, 
                'radial_mode_index': None, 
                # 'radial_mode_data': None
                }
    
        mode_nested_dictionary[i_gp][i_gp] = mode_dictionary
        # mode_dictionary = mode_nested_dictionary[ii][ii]
    
        #this loops through each row for the file
    
        for index,row in NumericalAxialWavenumberData_list[i_gp].iterrows(): 
    
            # TODO: improve plot_axial_wavenumbers to handle numerical cases
    
            # ax = pfcn.plot_axial_wavenumbers(
            #         real_part = NumericalAxialWavenumberData['Re{gam}'][i],
            #         imag_part = NumericalAxialWavenumberData['Im{gam}'][index],
            #         azimuthal_mode_number = azimuthal_mode_number,  
            #         radial_mode_number = index,
            #         wavenumber = wavenumber ,
            #         axial_mach_number = axial_mach_number,
            #         scatter_kwargs = scatter_parameters_numerical
            #         )
    
            # plt.show()
            # sys.exit()
    
            # print('directory',ii) 
            # i want to initialize the filtered wavenumbers 
            # filtered_wavenumber_real = NumericalAxialWavenumberData_list[ii]['Re{gam}'][index]
    
            ii = i_gp
            if NumericalAxialWavenumberData_list[ii]['Re{gam}'][index] == 0.0 and \
                    NumericalAxialWavenumberData_list[ii]['Im{gam}'][index] == 0.0:
                        # print('k_x = 0 , 0j, trivial, omitted')
                        pass
    
            elif NumericalAxialWavenumberData_list[ii]['Re{gam}'][index] < max_real_part and \
                    NumericalAxialWavenumberData_list[ii]['Im{gam}'][index] < max_imag_part and \
                    NumericalAxialWavenumberData_list[ii]['Im{gam}'][index] > min_imag_part and \
                    NumericalAxialWavenumberData_list[ii]['nz'][index] < num_of_zeros:
    
                filtered_wavenumber_real = NumericalAxialWavenumberData_list[ii]['Re{gam}'][index]
                filtered_wavenumber_imag = NumericalAxialWavenumberData_list[ii]['Im{gam}'][index]
    
                plt.scatter(
                        filtered_wavenumber_real,
                        filtered_wavenumber_imag,
                        marker = 'd',# changes triangle direction
                        # label = ii,
                        facecolor ='none', edgecolors ='b',
                        )
    
                if NumericalAxialWavenumberData_list[ii]['Im{gam}'][index] > 0 or \
                        NumericalAxialWavenumberData_list[ii]['Im{gam}'][index] < 0: 
    
                    ax.annotate(r'$K_{{{M},{N}}}$'.format(M = azimuthal_mode_number,N = NumericalAxialWavenumberData_list[ii]['nz'][index]),
                            xy=(
                                NumericalAxialWavenumberData_list[ii]['Re{gam}'][index],
                                NumericalAxialWavenumberData_list[ii]['Im{gam}'][index]
                                ),
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
                    ax.annotate(r'$K_{{{M},{N}}}$'.format(M = azimuthal_mode_number,N = NumericalAxialWavenumberData_list[ii]['nz'][index]),
                            xy=(
                                NumericalAxialWavenumberData_list[ii]['Re{gam}'][index],
                                NumericalAxialWavenumberData_list[ii]['Im{gam}'][index]),
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
                    
                    index_for_mode_import.append(NumericalAxialWavenumberData_list[ii]['#'][index]) 
                    # print(ii)
    
                if mode_dictionary['radial_mode_index'] == None:
                    # if there isn't a value for the radial mode index..  
                    mode_dictionary.update({
                        'k_x': (
                            NumericalAxialWavenumberData_list[ii]['Re{gam}'][index],
                            NumericalAxialWavenumberData_list[ii]['Im{gam}'][index]),
                        'radial_mode_number': NumericalAxialWavenumberData_list[ii]['nz'][index],
                        'radial_mode_index': NumericalAxialWavenumberData_list[ii]['#'][index],
                        }) 
    
                else:
                    fcn.append_value(mode_dictionary, 'k_x', (NumericalAxialWavenumberData_list[ii]['Re{gam}'][index], NumericalAxialWavenumberData_list[ii]['Im{gam}'][index]))
                    fcn.append_value(mode_dictionary, 'radial_mode_number', NumericalAxialWavenumberData_list[ii]['nz'][index])
                    fcn.append_value(mode_dictionary, 'radial_mode_index',  NumericalAxialWavenumberData_list[ii]['#'][index])
        
        plt.savefig(
            fname      ='figures/axial_wavenumber_analytical_test_case_comparison_{n}gridpoints_fourth_order.pdf'.format(
                n = str(grid_point_array[ii])),
            format     ='pdf')#, bbox_inches='tight')
        
        #------------------------------------------------------------------------------
        # identify modes from SWIRL that correspond to the computed radial wavenumber 
        
        len_indicies = len(mode_dictionary['radial_mode_index'])
        # pprint.pprint(mode_dictionary)

        # print('Identified Radial Mode Orders: ', mode_dictionary['radial_mode_number'])

        radial_mode_data_sets = {
                'k_x': None,
                'radial_mode_dataframe': None,
                } 
        radial_mode_filenames_per_grid = []
        radial_mode_filenames = []

        for i in range(len(mode_dictionary['radial_mode_index'])):

            if ii == 0:
                if mode_dictionary['radial_mode_index'][i] < 100:
                    # radial_mode_filenames.append('egv_np_0064radialmode_00' + str(mode_dictionary['radial_mode_index'][i])) 
                    radial_mode_filenames.append('egv_np_0032radialmode_00' + str(mode_dictionary['radial_mode_index'][i])) 
                else:
                    radial_mode_filenames.append('egv_np_0032radialmode_0' + str(mode_dictionary['radial_mode_index'][i]))

            if ii == 1:
                if mode_dictionary['radial_mode_index'][i] < 100:
                    # radial_mode_filenames.append('egv_np_0064radialmode_00' + str(mode_dictionary['radial_mode_index'][i])) 
                    radial_mode_filenames.append('egv_np_0064radialmode_00' + str(mode_dictionary['radial_mode_index'][i])) 
                else:
                    radial_mode_filenames.append('egv_np_0064radialmode_0' + str(mode_dictionary['radial_mode_index'][i]))

            if ii == 2:
                if mode_dictionary['radial_mode_index'][i] < 100:
                    # radial_mode_filenames.append('egv_np_0064radialmode_00' + str(mode_dictionary['radial_mode_index'][i])) 
                    radial_mode_filenames.append('egv_np_0128radialmode_00' + str(mode_dictionary['radial_mode_index'][i])) 
                else:
                    radial_mode_filenames.append('egv_np_0128radialmode_0' + str(mode_dictionary['radial_mode_index'][i]))

            if ii == 3:
                if mode_dictionary['radial_mode_index'][i] < 100:
                    # radial_mode_filenames.append('egv_np_0064radialmode_00' + str(mode_dictionary['radial_mode_index'][i])) 
                    radial_mode_filenames.append('egv_np_0256radialmode_00' + str(mode_dictionary['radial_mode_index'][i])) 
                else:
                    radial_mode_filenames.append('egv_np_0256radialmode_0' + str(mode_dictionary['radial_mode_index'][i])) 

        radial_mode_dataframe_dictionary = {}

        for i in range(len(mode_dictionary['radial_mode_index'])):
            radial_mode_data = \
                    pandas.read_csv(  fourth_order_directories[ii] + radial_mode_filenames[i], delim_whitespace = True )
            
            radial_mode_dataframe_dictionary[i] = radial_mode_data
        
        # pprint.pprint(radial_mode_dataframe_dictionary[1])
        
        dict_indicies = []
        numerical_normalized_radial_mode_list = []
        error_list = []
        L2_list = []
        L_max_list = []
        L_max_location_list = []
        
        for i,j in enumerate(mode_dictionary['radial_mode_number']):
            # print(radial_mode_number)
            # print(i,j)
            if j == radial_mode_number:
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
            # print(numerical_radial_mode)
        
            normalization_constant_numerical = afcn.normalize_psi((numerical_radial_mode),numerical_r) 
            # print(i,normalization_constant_numerical)
            normalized_radial_mode_data = normalization_constant_numerical*numerical_radial_mode
        
            numerical_normalized_radial_mode_list.append(normalized_radial_mode_data)

            error_list.append(abs(numerical_normalized_radial_mode_list[i][:].real-
                    analytical_normalized_radial_mode_list[radial_mode_number][:].real))

            # error_list.append(abs(numerical_normalized_radial_mode_list[i][1:-2].real-
            #         analytical_normalized_radial_mode_list[radial_mode_number][1:-2].real))
    
    
            # print(sum(error_list[i]))

            L2 = np.sqrt((1/grid_point_array[i_gp])*sum(error_list[i]**2))
            # print(L2) 

            L2_list.append(L2)
            # print('L2',L2_list[i])
            L_max = max(error_list[i]) 
            L_max_list.append(L_max)
            L_max_location = np.argmax(error_list[i])
            L_max_location_list.append(L_max_location)
            # L_max_location =error_list[i].index(L_max) 
            # print('L_max',L_max_list[i])
            # print('L_max_location',L_max_location_list[i])
            # print('L_max_location',L_max_location)

            # print((numerical_normalized_radial_mode_list[i]))
            # print((analytical_normalized_radial_mode_list[i]))
            # plt.show
    
        # print(type(normalized_radial_mode_data))
        # dict_indicies = [
        #         dict_index,
        #         dict_index+1,
        #         dict_index+2,
        #         dict_index+3]
            # for i in 1:
        
            fig,ax = plt.subplots(
                   constrained_layout=False,
                   figsize=fcn.set_size(345)
                   )
        
            plt.plot(
                   numerical_r,
                   normalized_radial_mode_data.real,
                   label = 'numerical,real')
        
            plt.plot(
                   numerical_r,
                   normalized_radial_mode_data.imag,
                   label = 'numerical,imag')
        
            plt.title('Radial mode '+ str(mode_dictionary['radial_mode_number'][dict_index]))
            plt.suptitle(str(mode_dictionary['k_x'][dict_index]))
        
            plt.plot(r,normalized_radial_mode_list[radial_mode_number].real, label='analytic,real',linestyle='dotted')
            plt.plot(r,normalized_radial_mode_list[radial_mode_number].imag, label='analytic,imag',linestyle='dotted')
            plt.legend()
            plt.xlabel('Radius')
            plt.ylabel('Pressure Fluctuation')
            plt.savefig(
                    fname      ='figures/fourth_order_radial_mode_{N}_test_case_number_{index}_grid_{n}.pdf'.format(
                        N = radial_mode_number,
                        index = i,
                        n = str(grid_point_array[ii])),
                    format     ='pdf')
    
            fig,ax = plt.subplots()
            
            plt.plot(r,error_list[i])
    
            plt.savefig(
                    fname      ='figures/fourth_order_radial_mode_error_{N}_test_case_number_{index}_grid_{n}.pdf'.format(
                        N = radial_mode_number,
                        index = i,
                        n = str(grid_point_array[ii])),
                    format     ='pdf')

        list_list.append(L2_list[0])
    for i in range(len(list_list)-1):
        
        ROC = np.log(list_list[i+1]/list_list[i])/np.log(0.5)#(grid_point_array[i+1]-grid_point_array[i])
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

print(list_list)

# CODE ENDS HERE

# pprint.pprint(mode_nested_dictionary)
# pprint.pprint(radial_mode_dataframe_dictionary)
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
