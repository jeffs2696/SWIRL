#!/usr/bin/env python3
import pprint
import pandas
import sys 
import math
import cmath
import numpy as np
import scipy as scp
from scipy import special
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

def append_value(dict_obj, key, value):
    # Check if key exist in dict or not
    if key in dict_obj:
        # Key exist in dict.
        # Check if type of value of key is list or not
        if not isinstance(dict_obj[key], list):

            # If type is not list then make it list
            dict_obj[key] = [dict_obj[key]]
            # Append the value in list
        dict_obj[key].append(value)
    else:
        # As key is not in dict,
        # so, add key-value pair
        dict_obj[key] = valueparameters

fcn.set_plot_parameters()


directories = [
        '../../../CodeRun/03-EVanalysis/SWIRLVerification/UniformFlowCylinderHardwall/FourthOrderDiff/32pts/']#, '../03-EVanalysis/SWIRLVerification/UniformFlowCylinderHardwall/FourthOrderDiff/64pts/', '../03-EVanalysis/SWIRLVerification/UniformFlowCylinderHardwall/FourthOrderDiff/128pts/', '../03-EVanalysis/SWIRLVerification/UniformFlowCylinderHardwall/FourthOrderDiff/256pts/', '../03-EVanalysis/SWIRLVerification/UniformFlowCylinderHardwall/FourthOrderDiff/512pts/']
filename = 'gam.nonconv_acc.0032'
NumericalAxialWavenumberData = \
            pandas.read_csv(  directories[0] + filename, delim_whitespace = True )
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

radial_mode_number = 0

axial_mach_number = 0.3
hub_to_tip_ratio = 0.25

angle = np.linspace(0,2*math.pi,100)

r_min = 0.25
r_max = 1
r_steps = 100
r = np.linspace(r_min,r_max,r_steps)

x_min = 0.25
x_max = 1
x_steps = 100
# x = np.linspace(x_min,x_max,x_steps)
t = 0

num_of_zeros = 5 # each zero corresponds to radial modes

Jv_p_zero = scp.special.jnp_zeros(n  = azimuthal_mode_number, nt = num_of_zeros)
Jv_p_zero_01 = scp.special.jnp_zeros(n  = 0, nt = 1)

k_x_plus,k_x_minus = fcn.axial_wavenumber_quadratic(wavenumber,axial_mach_number,Jv_p_zero)

if azimuthal_mode_number < 1:
    # append wavenumber (frequency) as the first wavenumber when m = 0 
    k_x_plus = np.append(wavenumber,k_x_plus)
    k_x_minus = np.append(-wavenumber,k_x_minus)

radial_mode = scp.special.jv(
        azimuthal_mode_number,
        Jv_p_zero[radial_mode_number]*r)

normalization_constant = fcn.normalize_psi((radial_mode),r)
normalized_radial_mode = normalization_constant*radial_mode

if azimuthal_mode_number == 0 and radial_mode_number == 1:
    analytical_normalization_constant = np.sqrt(2) #from Fund. Of Duct Acs - Rienstra
else: 
    analytical_normalization_constant = np.sqrt(2)/(abs(scp.special.jv(azimuthal_mode_number, Jv_p_zero[radial_mode_number]))*np.sqrt(1 - azimuthal_mode_number**2/Jv_p_zero[radial_mode_number]**2))

analytical_normalized_radial_mode = analytical_normalization_constant*(radial_mode)

# 
# -----------------------------------------------------------------------------
# plotting data 

fig,ax = plt.subplots(
        constrained_layout=False,
        # figsize=fcn.set_size(345)
)
for i,j in enumerate(Jv_p_zero):

    plt.scatter(k_x_plus[i].real,k_x_plus[i].imag, marker ='.' , c='black')
    plt.scatter(k_x_minus[i].real,k_x_minus[i].imag, marker ='.' , c='black')
    # print(k_x_imag_plus)
    if k_x_plus[i].imag > 0 or k_x_plus[i].imag < 0:
        ax.annotate(r'$k^+_{{{M},{N}}}$'.format(M = azimuthal_mode_number,N = i),
                xy=(
                    k_x_plus[i].real - k_x_plus[i].real/5,
                    k_x_plus[i].imag
                    ),
                xycoords='data',
                fontsize='8')
        ax.annotate(r'$k^-_{{{M},{N}}}$'.format(M = azimuthal_mode_number,N = i),
                xy=(
                    k_x_minus[i].real - k_x_minus[i].real/5,
                    k_x_minus[i].imag
                    ),
                xycoords='data',
                fontsize='8')
    else:
        ax.annotate(r'$k^+_{{{M},{N}}}$'.format(M = azimuthal_mode_number,N = i),
                xy=(
                    k_x_plus[i].real ,
                    k_x_plus[i].imag- k_x_plus[i].imag/5
                    ),
                xycoords='data',
                fontsize='8')
        ax.annotate(r'$k^-_{{{M},{N}}}$'.format(M = azimuthal_mode_number,N = i),
                xy=(k_x_minus[i].real,k_x_minus[i].imag),
                xycoords='data',
                fontsize='8')

if axial_mach_number > 0:        
    k_x_cutoff = axial_mach_number*wavenumber/(axial_mach_number**2-1)
    plt.axvline(x = k_x_cutoff,color = 'black', label = 'cut-off line',lw=1) 
    plt.legend()

txt = "Complex axial wavenumbers for $m = 0$, $k = 5$, $M_x = 0$ $\eta = 0$"

plt.xlabel(r'\begin{center}\textit{Real}$(k_x)$\\*\textit{\small{' + txt + r'}}\end{center}')
plt.ylabel(r'\textit{Imaginary}$(k_x)$')
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

mode_dictionary = {
        'k_x' : None,
        'radial_mode': None, 
        'radial_mode_index': None 
        }

for i,row in NumericalAxialWavenumberData.iterrows(): 
    # print(NumericalAxialWavenumberData['Re{gam}'][i])
    if NumericalAxialWavenumberData['Re{gam}'][i] < max_real_part and NumericalAxialWavenumberData['Im{gam}'][i] < max_imag_part and NumericalAxialWavenumberData['Im{gam}'][i] > min_imag_part:
        plt.scatter(
                NumericalAxialWavenumberData['Re{gam}'][i],
                NumericalAxialWavenumberData['Im{gam}'][i], 
                marker = 'd',
                facecolor = 'none', edgecolors ='b',
                ) 

        if NumericalAxialWavenumberData['Im{gam}'][i] > 0 or NumericalAxialWavenumberData['Im{gam}'][i] < 0:
            ax.annotate(r'$t^-_{{{M},{N}}}$'.format(M = azimuthal_mode_number,N = NumericalAxialWavenumberData['nz'][i]),
                    xy=(
                        NumericalAxialWavenumberData['Re{gam}'][i]+NumericalAxialWavenumberData['Re{gam}'][i]/3,
                        NumericalAxialWavenumberData['Im{gam}'][i]
                        ),
                    xycoords='data',
                    fontsize='8')
        else:
            ax.annotate(r'$t^-_{{{M},{N}}}$'.format(M = azimuthal_mode_number,N = NumericalAxialWavenumberData['nz'][i]),
            xy=(
                    NumericalAxialWavenumberData['Re{gam}'][i]+NumericalAxialWavenumberData['Re{gam}'][i]/3,
                    NumericalAxialWavenumberData['Im{gam}'][i]
                    ),
                    xycoords='data',
                    fontsize='8')

        index_for_mode_import.append(NumericalAxialWavenumberData['#'][i]) 
        # mode_dictionary["index"] =NumericalAxialWavenumberData['#'][i] 
        if mode_dictionary['radial_mode_index'] == None:
            mode_dictionary.update({
                'k_x': (NumericalAxialWavenumberData['Re{gam}'][i],NumericalAxialWavenumberData['Im{gam}'][i]),
                'radial_mode': NumericalAxialWavenumberData['nz'][i],
                'radial_mode_index': NumericalAxialWavenumberData['#'][i],
                }) 
        else:
            append_value(mode_dictionary, 'k_x', (NumericalAxialWavenumberData['Re{gam}'][i], NumericalAxialWavenumberData['Im{gam}'][i]))
            append_value(mode_dictionary, 'radial_mode', NumericalAxialWavenumberData['nz'][i])
            append_value(mode_dictionary, 'radial_mode_index', NumericalAxialWavenumberData['#'][i])

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

len_indicies = len(mode_dictionary['radial_mode_index'])
# pprint.pprint(mode_dictionary)
print('Identified Radial Mode Orders: ', mode_dictionary['radial_mode'])

radial_mode_data_sets = {
        'k_x': None,
        'radial_mode_dataframe': None,
        } 
radial_mode_filenames = []
for i in range(len(mode_dictionary['radial_mode_index'])):

    if mode_dictionary['radial_mode_index'][i] < 100:
        radial_mode_filenames.append('egv_np_0032radialmode_00' + str(mode_dictionary['radial_mode_index'][i])) 
    else:
        radial_mode_filenames.append('egv_np_0032radialmode_0' + str(mode_dictionary['radial_mode_index'][i])) 

radial_mode_dataframe_dictionary = {}

for i in range(len(mode_dictionary['radial_mode_index'])):
    radial_mode_data = \
            pandas.read_csv(  directories[0] + radial_mode_filenames[i], delim_whitespace = True )
    
    radial_mode_dataframe_dictionary[i] = radial_mode_data

# pprint.pprint(radial_mode_dataframe_dictionary[0])
dict_index = 0#len_indicies 
numerical_r = radial_mode_dataframe_dictionary[dict_index]['Rad']

numerical_radial_mode_real = radial_mode_dataframe_dictionary[dict_index]['p_no_phase[Re]']
numerical_radial_mode_imag = radial_mode_dataframe_dictionary[dict_index]['p_no_phase[Im]']

numerical_radial_mode = numerical_radial_mode_real.to_numpy() + numerical_radial_mode_imag.to_numpy()*1j 

normalization_constant_numerical = fcn.normalize_psi((numerical_radial_mode),numerical_r) 
normalized_radial_mode_data = normalization_constant_numerical*numerical_radial_mode


# print(type(normalized_radial_mode_data))
# dict_indicies = [
#         dict_index,
#         dict_index+1,
#         dict_index+2,
#         dict_index+3]


# for i in 1:
fig,ax = plt.subplots(
       constrained_layout=False)
plt.plot(
       numerical_r,
       normalized_radial_mode_data.real,
       label = 'numerical,real')
plt.plot(
       numerical_r,
       normalized_radial_mode_data.imag,
       label = 'numerical,imag')

plt.title('Radial mode '+ str(mode_dictionary['radial_mode'][dict_index]))
plt.suptitle(str(mode_dictionary['k_x'][dict_index]))

plt.plot(r,normalized_radial_mode.real, label='analytic,real',linestyle='dotted')
plt.plot(r,normalized_radial_mode.imag, label='analytic,imag',linestyle='dotted')
plt.legend()
plt.xlabel('Radius')
plt.ylabel('Pressure Fluctuation')
plt.show()
sys.exit()
# resize the figure to match the aspect ratio of the Axes    
fig.set_size_inches(10, 8, forward=True)
plt.savefig(
    fname      ='figures/axial_wavenumber_analytical_test_case.pdf',
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
