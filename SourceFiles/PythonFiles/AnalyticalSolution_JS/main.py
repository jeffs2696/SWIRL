#!/usr/bin/env python3
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

def count_keys(dict_, counter=0):
    for each_key in dict_:
        if isinstance(dict_[each_key], dict):
            # Recursive call
            counter = count_keys(dict_[each_key], counter + 1)
        else:
            counter += 1
    return counter

fcn.set_plot_parameters()

# Currently this code obtains the analytical solution for a uniform flow 
# cylinder

# [1] importing results from SWIRL
directories = [
        '../../../CodeRun/03-EVanalysis/SWIRLVerification/UniformFlowCylinderHardwall/FourthOrderDiff/32pts/',
        '../../../CodeRun/03-EVanalysis/SWIRLVerification/UniformFlowCylinderHardwall/FourthOrderDiff/64pts/',
        '../../../CodeRun/03-EVanalysis/SWIRLVerification/UniformFlowCylinderHardwall/FourthOrderDiff/128pts/',
        '../../../CodeRun/03-EVanalysis/SWIRLVerification/UniformFlowCylinderHardwall/FourthOrderDiff/256pts/']#, '../../../CodeRun/03-EVanalysis/SWIRLVerification/UniformFlowCylinderHardwall/FourthOrderDiff/512pts/']

filename = 'gam.nonconv_acc.0032'
filename_list = [
        'gam.nonconv_acc.0032',
        'gam.nonconv_acc.0064',
        'gam.nonconv_acc.0128',
        'gam.nonconv_acc.0256' ]


NumericalAxialWavenumberData = \
            pandas.read_csv(  directories[1] + filename_list[1], delim_whitespace = True )

NumericalAxialWavenumberData_list = []
for i in range(len(directories)):
    NumericalAxialWavenumberData = \
            pandas.read_csv(  directories[i] + filename_list[i], delim_whitespace = True )
    NumericalAxialWavenumberData_list.append(NumericalAxialWavenumberData)

NumericalAxialWavenumberData = NumericalAxialWavenumberData_list[0]

# Initializing list

# sys.exit()
# NumericalAxialWavenumberData1 = fcn.importPlotData(
#         directories[1] + 'gam.nonconv_acc.0064')


# NumericalAxialWavenumberData2 = fcn.importPlotData(
#         directories[2] + 'gam.nonconv_acc.0128')

# NumericalAxialWavenumberData3 = fcn.importPlotData(
#         directories[3] + 'gam.nonconv_acc.0256')

# test case parameters

# no flow
wavenumber = 10 

azimuthal_mode_number = 2 

radial_mode_number =2

axial_mach_number = 0.3
hub_to_tip_ratio = 0.25

angle = np.linspace(0,2*math.pi,100)

r_min = 0
r_max = 1
r_steps = 100
r = np.linspace(r_min,r_max,r_steps)

x_min = 0.0
x_max = 1
x_steps = 100
# x = np.linspace(x_min,x_max,x_steps)
t = 0

num_of_zeros = 5 # each zero corresponds to radial modes
Jv_p_zero = afcn.get_radial_wavenumbers(azimuthal_mode_number,num_of_zeros)

# Yv_p_zero = scp.special.ynp_zeros(n  = azimuthal_mode_number, nt = num_of_zeros)
Jv_p_zero_01 = scp.special.jnp_zeros(n  = 0, nt = 1)

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

normalized_radial_mode_list = []
analytical_normalized_radial_mode_list = []

# normalizing radial modes in a loop
for i,j in enumerate(radial_mode_list):

    radial_mode = radial_mode_list[i]
    radial_mode_number_for_list = i
    normalization_constant = afcn.normalize_psi((radial_mode),r)
    normalized_radial_mode = normalization_constant*radial_mode
    normalized_radial_mode_list.append(normalized_radial_mode) 

    if azimuthal_mode_number == 0 and radial_mode_number_for_list == 0:
        #from Fund. Of Duct Acs - Rienstra 
        analytical_normalization_constant = np.sqrt(2)     
    else: 
        analytical_normalization_constant = np.sqrt(2)/(
                (
                    abs(scp.special.jv(
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
width = 500 # to match page width in TeX doc

fig,ax = plt.subplots(
        constrained_layout=False,
        figsize=fcn.set_size(width)
)
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

#------------------------------------------------------------------------------
# 
if axial_mach_number > 0:
    k_x_convective = wavenumber/axial_mach_number
    max_real_part = 33 #k_x_convective
    max_imag_part = 14
    min_imag_part = -14
else: 
    max_real_part = 33
    max_imag_part = 14
    min_imag_part = -14

index_for_mode_import = []
key_list_for_mode_dictionary =[ "radial_mode", "index"] 

# Import modes from SWIRL, (psuedocode for FORTRAN
mode_dictionary = {
        'k_x' : None,
        'radial_mode_number': None, 
        'radial_mode_index': None 
        }

mode_nested_dictionary = {}

for i in range(len(directories)):
    mode_nested_dictionary[i] = {i: mode_dictionary}
    

# pprint.pprint((mode_nested_dictionary[1][1]['radial_mode_index']))
for ii in range(0,1): 

    # mode_dictionary = mode_nested_dictionary[ii][ii]

    for i,row in NumericalAxialWavenumberData_list[ii].iterrows(): 
        # print(NumericalAxialWavenumberData['Re{gam}'][i]r
        # TODO: improve plot_axial_wavenumbers to handle numerical cases
        # ax = pfcn.plot_axial_wavenumbers(
        #         real_part = NumericalAxialWavenumberData['Re{gam}'][i],
        #         imag_part = NumericalAxialWavenumberData['Im{gam}'][i],
        #         azimuthal_mode_number = azimuthal_mode_number,  
        #         radial_mode_number = i,
        #         wavenumber = wavenumber ,
        #         axial_mach_number = axial_mach_number,
        #         scatter_kwargs = scatter_parameters_numerical
        #         )
        
        # plt.show()
        # sys.exit()
        
        # print('directory',ii)
        
        if NumericalAxialWavenumberData_list[ii]['Re{gam}'][i] == 0.0 and NumericalAxialWavenumberData_list[ii]['Im{gam}'][i] == 0.0:
            print('k_x = 0 , 0j, trivial')
        elif NumericalAxialWavenumberData_list[ii]['Re{gam}'][i] < max_real_part and NumericalAxialWavenumberData_list[ii]['Im{gam}'][i] < max_imag_part and NumericalAxialWavenumberData_list[ii]['Im{gam}'][i] > min_imag_part:
            plt.scatter(
                    NumericalAxialWavenumberData_list[ii]['Re{gam}'][i],
                    NumericalAxialWavenumberData_list[ii]['Im{gam}'][i], 
                    marker = 'd',# changes triangle direction
                    # label = ii,
                    facecolor ='none', edgecolors ='b',
                    )
            
            if NumericalAxialWavenumberData_list[ii]['Im{gam}'][i] > 0 or NumericalAxialWavenumberData_list[ii]['Im{gam}'][i] < 0: 
                ax.annotate(r'$K_{{{M},{N}}}$'.format(M = azimuthal_mode_number,N = NumericalAxialWavenumberData_list[ii]['nz'][i]),
                        xy=(
                            NumericalAxialWavenumberData_list[ii]['Re{gam}'][i],#+NumericalAxialWavenumberData['Re{gam}'][i]/3,
                            NumericalAxialWavenumberData_list[ii]['Im{gam}'][i]
                            ),
                        xycoords='data',
                        textcoords='offset points',
                        xytext = (-10,-20),
                        horizontalalignment='center',
                        verticalalignment='bottom',
                        fontsize='10',
                        arrowprops=dict(arrowstyle= '-',
                            color='blue',
                            lw=1,
                            ls='--')
                        )
            else:
                ax.annotate(r'$K_{{{M},{N}}}$'.format(M = azimuthal_mode_number,N = NumericalAxialWavenumberData_list[ii]['nz'][i]),
                        xy=(
                            NumericalAxialWavenumberData_list[ii]['Re{gam}'][i],
                            NumericalAxialWavenumberData_list[ii]['Im{gam}'][i]),
                        xycoords='data',
                        textcoords='offset points',
                        xytext = (0,-20),
                        horizontalalignment='right',
                        verticalalignment='bottom',
                        fontsize='10',
                        arrowprops=dict(arrowstyle= '-',
                            color='blue',
                            lw=1,
                            ls='--')
                        )
                
                index_for_mode_import.append(NumericalAxialWavenumberData_list[ii]['#'][i]) 
                print(ii)
                if mode_dictionary['radial_mode_index'] == None:
                    # if there isn't a value for the radial mode index..  
                    mode_dictionary.update({
                        'k_x': (
                            NumericalAxialWavenumberData_list[ii]['Re{gam}'][i],
                            NumericalAxialWavenumberData_list[ii]['Im{gam}'][i]),
                        'radial_mode_number': NumericalAxialWavenumberData_list[ii]['nz'][i],
                        'radial_mode_index': NumericalAxialWavenumberData_list[ii]['#'][i],
                        }) 
                else:
                    fcn.append_value(mode_dictionary, 'k_x', (NumericalAxialWavenumberData_list[ii]['Re{gam}'][i], NumericalAxialWavenumberData_list[ii]['Im{gam}'][i]))
                    fcn.append_value(mode_dictionary, 'radial_mode_number', NumericalAxialWavenumberData_list[ii]['nz'][i])
                    fcn.append_value(mode_dictionary, 'radial_mode_index',  NumericalAxialWavenumberData_list[ii]['#'][i])
                    

#                 if mode_nested_dictionary[ii][ii]['radial_mode_index'] == None:
#                     # if there isn't a value for the radial mode index..  
#                     mode_nested_dictionary[ii][ii].update({
#                         'k_x': (
#                             NumericalAxialWavenumberData_list[ii]['Re{gam}'][i],
#                             NumericalAxialWavenumberData_list[ii]['Im{gam}'][i]),
#                         'radial_mode_number': NumericalAxialWavenumberData_list[ii]['nz'][i],
#                         'radial_mode_index': NumericalAxialWavenumberData_list[ii]['#'][i],
#                         }) 
#                 else:
#                     fcn.append_value(mode_nested_dictionary[ii][ii], 'k_x', (NumericalAxialWavenumberData_list[ii]['Re{gam}'][i], NumericalAxialWavenumberData_list[ii]['Im{gam}'][i]))
#                     fcn.append_value(mode_nested_dictionary[ii][ii], 'radial_mode_number', NumericalAxialWavenumberData_list[ii]['nz'][i])
#                     fcn.append_value(mode_nested_dictionary[ii][ii], 'radial_mode_index',  NumericalAxialWavenumberData_list[ii]['#'][i])
                    

                    

# sys.exit()
plt.savefig(
    fname      ='figures/axial_wavenumber_analytical_test_case_comparison.pdf',
    format     ='pdf',
    bbox_inches='tight')

        # print(mode_dictionary['radial_mode_index'])
        # print(
        #         NumericalAxialWavenumberData['Re{gam}'][i],
        #         NumericalAxialWavenumberData['Im{gam}'][i],
        #         'index',NumericalAxialWavenumberData['#'][i],
        #         'radial_mode',NumericalAxialWavenumberData['nz'][i])

        # ax.annotate(r'$t^_{{{M},{N}}}$'.format(M = azimuthal_mode_number,N = int(NumericalAxialWavenumberData['nz'][i])),
        #         xy=(NumericalAxialWavenumberData['Re{gam}'],
        #             NumericalAxialWavenumberData['Im{gam}']),
        #         xycoords='data',
        #         fontsize='8')
#------------------------------------------------------------------------------
# identify modes from SWIRL that correspond to the computed radial wavenumber

plt.legend()
# plt.show()
# pprint.pprint(mode_nested_dictionary)

len_indicies = len(mode_dictionary['radial_mode_index'])
# pprint.pprint(mode_dictionary)
print('Identified Radial Mode Orders: ', mode_dictionary['radial_mode_number'])

radial_mode_data_sets = {
        'k_x': None,
        'radial_mode_dataframe': None,
        } 
radial_mode_filenames_per_grid = []
radial_mode_filenames = []

for i in range(len(mode_dictionary['radial_mode_index'])):

    if mode_dictionary['radial_mode_index'][i] < 100:
        # radial_mode_filenames.append('egv_np_0064radialmode_00' + str(mode_dictionary['radial_mode_index'][i])) 
        radial_mode_filenames.append('egv_np_0032radialmode_00' + str(mode_dictionary['radial_mode_index'][i])) 
    else:
        radial_mode_filenames.append('egv_np_0032radialmode_0' + str(mode_dictionary['radial_mode_index'][i])) 

radial_mode_dataframe_dictionary = {}

for i in range(len(mode_dictionary['radial_mode_index'])):
    radial_mode_data = \
            pandas.read_csv(  directories[0] + radial_mode_filenames[i], delim_whitespace = True )
    
    radial_mode_dataframe_dictionary[i] = radial_mode_data

# pprint.pprint(radial_mode_dataframe_dictionary[1])

dict_indicies = []
for i,j in enumerate(mode_dictionary['radial_mode_number']):
    # print(radial_mode_number)
    # print(i,j)
    if j == radial_mode_number:
        print('match!',j,'at',i)
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
    #---------------------------
    plt.show()
sys.exit()
# resize the figure to match the aspect ratio of the Axes    
fig.set_size_inches(10, 8, forward=True)
plt.savefig(
    fname      ='figures/radial_mode_0_test_case.pdf',
    format     ='pdf',
    bbox_inches='tight')

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
