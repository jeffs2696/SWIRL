#!/usr/bin/env python3
import sys
sys.path.append("..")
import helpers.logging_functions as lfcn
import helpers.math_functions as mfcn
import helpers.helper_functions as fcn
import matplotlib
import matplotlib.pyplot as plt
import pandas as pd
import pprint 
import scipy as scip
import numpy as np

fcn.set_plot_parameters() #plot parameters
logger = lfcn.log_to_console()
debug_flag = True
grid_point_array = [121, 161, 201, 241, 281, 321, 361, 401, 441,481, 521,561]
L2_list = []    
L2_list_D2 = []    
L2_list_D4 = []    
L2_list_D6 = []    
L2_list_D8 = []    
L2_list_D10 = []    
x_data_total_list = []
y_data_total_list = []
y2_data_total_list = []

for i_gp,j_gp in enumerate(grid_point_array):
    y_data_list = []
    y2_data_list = []
    x_data_list = []
    for i in range(0,5): 
        filename = f'fourth_order_study/SWIRL_Dissipation_nD_{i+1}_{j_gp}_fd2.dat'.format()
        # print(filename)
    
        data = \
                pd.read_csv(filename, delim_whitespace = True)
        
        x_data_list.append(data['domain'])
        y_data_list.append(data['no_dissipation'])
        y2_data_list.append(data['dissipation'])
        x_data_total_list.append(x_data_list)
        y_data_total_list.append(y_data_list)
        y2_data_total_list.append(y2_data_list)

        if debug_flag:
           pprint.pprint(data)

    
    
        # sys.exit()
        grid = x_data_list
        error = y_data_list
        error_w_dissipation = y2_data_list

    # if plot_mode_data_flag: 
    
    # fig, axs = plt.subplots()
    # # print(y_data_list)
    # # sys.exit()
    # # plt.axis([0.5,1,0, 0.0000001])
    # plt.suptitle('Radial Mode Error Dissipation Comparison')

    # # plt.plot( 
    # #         x_data_list[i],
    # #         error[i]  ,
    # #         linewidth = 2,
    # #         label = '$\epsilon$',
    # #         )
    # # plt.plot( 
    # #         x_data_list[i],
    # #         error_w_dissipation[0]  ,
    # #         linewidth = 2, 
    # #         linestyle = 'dotted',
    # #         label= r'D2') 
    # plt.plot( 
    #         x_data_list[i],
    #         error_w_dissipation[1]  ,
    #         linewidth = 2,
    #         linestyle = 'dotted',
    #         label = r'D4')
    # plt.plot( 
    #         x_data_list[i],
    #         error_w_dissipation[2]  ,
    #         linewidth = 2,
    #         linestyle = 'dotted',
    #         label = r'D6')
    # plt.plot( 
    #         x_data_list[i],
    #         error_w_dissipation[3]  ,
    #         linewidth = 2,
    #         marker = '.',
    #         linestyle = 'dashdot',
    #         label = r'D8')
    # plt.plot( 
    #         x_data_list[i],
    #         error_w_dissipation[4]  ,
    #         linewidth = 2,
    #         linestyle = 'dashed',
    #         label = r'D10')
    # plt.legend()
    # a = plt.axes([0.2,0.6, 0.2,0.2])
    # index_from_bc = -6
    # # plt.plot(x_data_list[0][index_from_bc:],
    # #         error[0][index_from_bc:],
    # #         linewidth = 2)
    # plt.plot(x_data_list[0][index_from_bc:],
    #         error_w_dissipation[0][index_from_bc:],
    #         linewidth = 2,
    #         linestyle = 'dotted')
    # plt.plot(x_data_list[1][index_from_bc:],
    #         error_w_dissipation[1][index_from_bc:],
    #         linewidth = 2,
    #         linestyle = 'dotted')
    # plt.plot(x_data_list[2][index_from_bc:],
    #         error_w_dissipation[2][index_from_bc:],
    #         linewidth = 2,
    #         linestyle = 'dotted')
    # plt.plot(x_data_list[3][index_from_bc:],
    #         error_w_dissipation[3][index_from_bc:],
    #         linewidth = 2,
    #         marker = '.',
    #         linestyle = 'dashdot')
    # plt.plot(x_data_list[4][index_from_bc:],
    #         error_w_dissipation[4][index_from_bc:],
    #         linewidth = 2,
    #         linestyle = 'dashed')
    L2_list.append(np.sqrt(sum(1/len(error[0])*error[0]**2)))
    L2_list_D2.append(np.sqrt(sum(1/len(error_w_dissipation[0])*error_w_dissipation[0]**2)))
    L2_list_D4.append(np.sqrt(sum(1/len(error_w_dissipation[1])*error_w_dissipation[1]**2)))
    L2_list_D6.append(np.sqrt(sum(1/len(error_w_dissipation[2])*error_w_dissipation[2]**2)))
    L2_list_D8.append(np.sqrt(sum(1/len(error_w_dissipation[3])*error_w_dissipation[3]**2)))
    L2_list_D10.append(np.sqrt(sum(1/len(error_w_dissipation[4])*error_w_dissipation[4]**2)))
    # plt.savefig(
    #         f'SWIRL_Dissipation_nD_{i+1}_{j_gp}_fd2.pdf'.format(),format='pdf')


    # plt.plot(x_data_list[0],error_w_dissipation[0])

    # plt.plot(x_data_list[1],(y_data_list[1] + y2_data_list[1]))
    # plt.plot(x_data_list[2],(y_data_list[2] + y2_data_list[2]))
    # plt.plot(x_data_list[3],(y_data_list[3] + y2_data_list[3]))
    # plt.plot(x_data_list[4],(y_data_list[4] + y_data_list[4]))

ROC_list=[]
ROC_list_D2=[]
ROC_list_D4=[]
ROC_list_D6=[]
ROC_list_D8=[]
ROC_list_D10=[]
for i in range(len(grid_point_array)-1): 
    refinement_ratio = grid_point_array[i+1]/grid_point_array[i] # 2 for a doubling of gridpoints 
    ROC = np.log(
            L2_list[i]/L2_list[i+1])/\
                    np.log(refinement_ratio) 
    ROC_list.append(ROC)
    ROC_D2 = np.log(
            L2_list_D2[i]/L2_list_D2[i+1])/\
                    np.log(refinement_ratio) 
    ROC_list_D2.append(ROC_D2)
    ROC_D4 = np.log(
            L2_list_D4[i]/L2_list_D4[i+1])/\
                    np.log(refinement_ratio) 
    ROC_list_D4.append(ROC_D4)
    ROC_D6 = np.log(
            L2_list_D6[i]/L2_list_D6[i+1])/\
                    np.log(refinement_ratio) 
    ROC_list_D6.append(ROC_D6)
    ROC_D8 = np.log(
            L2_list_D8[i]/L2_list_D8[i+1])/\
                    np.log(refinement_ratio) 
    ROC_list_D8.append(ROC_D8)
    ROC_D10 = np.log(
            L2_list_D10[i]/L2_list_D10[i+1])/\
                    np.log(refinement_ratio) 
    ROC_list_D10.append(ROC_D10)
fig, ax = plt.subplots(
        nrows=1,
        ncols=1,
        sharex=True)
ax.loglog(
        grid_point_array,
        L2_list_D2,
        marker = '+',
        linewidth = 2, 
        markersize = 10,
        label = 'D2'
        )
ax.loglog(
        grid_point_array,
        L2_list_D4,
        marker = '+',
        markersize = 10,
        label = 'D4'
        )
ax.loglog(
        grid_point_array,
        L2_list_D6,
        marker = '+',
        linewidth = 2, 
        markersize = 10,
        label = 'D6'
        )
ax.loglog(
        grid_point_array ,
        L2_list_D8,
        marker = '+',
        linewidth = 2, 
        markersize = 10,
        label = 'D8'
        )
ax.loglog(
        grid_point_array,
        L2_list_D10,
        marker = '+',
        linewidth = 2, 
        markersize = 10,
        label = 'D10'
        )
ax.set_ylabel(r'Rate of Convergence, $\alpha$') 
ax.set_xlabel('index')
plt.legend()
        

fig, ax = plt.subplots(
        nrows=1,
        ncols=1,
        sharex=True)
ax.semilogx(
        ROC_list_D2,
        marker = '+',
        linewidth = 2, 
        markersize = 10,
        label = 'D2'
        )
ax.semilogx(
        ROC_list_D4,
        marker = '+',
        linewidth = 2, 
        markersize = 10,
        label = 'D4'
        )
ax.semilogx(
        ROC_list_D6,
        marker = '+',
        linewidth = 2, 
        markersize = 10,
        label = 'D6'
        )
ax.semilogx(
        ROC_list_D8 ,
        marker = '+',
        linewidth = 2, 
        markersize = 10,
        label = 'D8'
        )
ax.semilogx(
        ROC_list_D10 ,
        marker = '+',
        linewidth = 2, 
        markersize = 10,
        label = 'D10'
        )
ax.set_ylabel(r'Rate of Convergence, $\alpha$') 
ax.set_xlabel('index')
plt.legend()
        

print(ROC_list)
print(ROC_list_D2)
print(ROC_list_D4)
print(ROC_list_D6)
print(ROC_list_D8)
print(ROC_list_D10)
# print(L2_list)

# print(L2_list_D2)
# print(L2_list_D4)
# print(L2_list_D6)
# print(L2_list_D8)
# print(L2_list_D10)
# fig,axs = plt.subplots()
# plt.plot(    
# x_data_total_list[1][1],
# y_data_total_list[1][1])
        
        
        
        
plt.show()
        
        
        
