# ========= Packages ========
import sympy as sp 
import numpy as np
import matplotlib.pyplot as mpl
from IPython.display import Markdown, display
from sympy import pprint
from sympy.interactive import printing
printing.init_printing(use_latex = True) 
from numpy import linspace
# ======== Functions ======== 

def ModifiedManufacturedSolution(f_MS, \
                    f_minBC,\
                    f_maxBC,\
                    f_minMS,\
                    f_maxMS,\
                    A_min, \
                    A_max):
    
    
    f_BCsImposed  = f_MS + \
    A_min*(f_minBC - f_minMS) + \
    A_max*(f_maxBC - f_maxMS)
    #print(f_BCsImposed)
    # Substitute the symbols for what ever they are currently defined
    f_BCsImposed  = f_BCsImposed.subs(\
                                     [(sp.Symbol('f_MS')    ,f_MS), \
                                      (sp.Symbol('f_minBC'),f_minBC),\
                                      (sp.Symbol('f_maxBC'),f_maxBC), \
                                      (sp.Symbol('f_minMS'),f_minMS), \
                                      (sp.Symbol('f_maxMS'),f_maxMS), \
                                      (sp.Symbol('A_min'),A_min), \
                                      (sp.Symbol('A_max'),A_max) ])
    
    return f_BCsImposed

def diffModifiedManufacturedSolution(f_MS, \
                    del_f_minBC,\
                    del_f_maxBC,\
                    B_min, \
                    B_max):
    
    
    f_BCsImposed  = f_MS + \
    B_min*(del_f_minBC ) + \
    B_max*(del_f_maxBC )
    
    # Substitute the symbols for what ever they are currently defined
    f_BCsImposed  = f_BCsImposed.subs(\
                                     [(sp.Symbol('f_MS')    ,f_MS), \
                                      (sp.Symbol('del_f_minBC'),del_f_minBC),\
                                      (sp.Symbol('del_f_maxBC'),del_f_maxBC), \
                                      (sp.Symbol('B_min'),B_min), \
                                      (sp.Symbol('B_max'),B_max) ])
    return f_BCsImposed
#
#def diffModifiedManufacturedSolution(f_MS, \
#                    df_minBC,\
#                    df_maxBC,\
#                    df_minMS,\
#                    df_maxMS,\
#                    B_min, \
#                    B_max):
#    
#    
#    f_BCsImposed  = f_MS + \
#    B_min*(df_minBC - df_minMS) + \
#    B_max*(df_maxBC - df_maxMS)
#    
#    # Substitute the symbols for what ever they are currently defined
#    f_BCsImposed  = f_BCsImposed.subs(\
#                                     [(sp.Symbol('f_MS')    ,f_MS), \
#                                      (sp.Symbol('df_minBC'),df_minBC),\
#                                      (sp.Symbol('df_maxBC'),df_maxBC), \
#                                      (sp.Symbol('df_minMS'),df_minMS), \
#                                      (sp.Symbol('df_maxMS'),df_maxMS), \
#                                      (sp.Symbol('B_min'),B_min), \
#                                      (sp.Symbol('B_max'),B_max) ])
#    return f_BCsImposed

def TanhMethod(n,B,r_min,r_max):
    # inputs: 
    #   n - number of tanh functions
    #   B - the slope around the inflection point
    
    # outputs:     
    # initialize lists for the tanh function
    RightKink  = []
    LeftKink   = []
   
    # symbolic variables needed for this function
    r = sp.Symbol('r')
    
    # rescaling the radius (redundant but needed for BC Fairing Function)
    r_hat = (r - r_min)/(r_max - r_min)    
    
    # amplitude for each wave
    A          = []
    
    # maximum allowed amplitude 
    max_amplitude = 0.75
   
    # vertical shift along the y axis
    # one is chosed to keep the inflection points above 
    # zero
    S_vertical = 1
    
    # rj is the list of inflection point locations
    rj = list(linspace(1,0,n))


    
    # messages for error and warning checking 
    warning_mssg = {1:'Warning: Total Amplitude exceeds maximum ', \
                    2:'Warning: Function is negative'}
    
    # getting amplitude for each kink
    for i in range(len(rj)):
        # 
        A.append(max_amplitude/(len(rj)+1))
        if sum(A) > max_amplitude:
            sys.exit(str(warning_mssg[1]))
    
    # defining kinks and antikinks
    for j in range(len(rj)):
        RightKink.append( A[j]*sp.tanh(B*( r_hat    - rj[j] )) )
        LeftKink.append(  A[j]*sp.tanh(B*( rj[j]- rj[0] )) )   
    
    f = sum(LeftKink) + sum(RightKink) + S_vertical
    return f

def plotSymbolicEquation(x_data,y_data,x_min,x_max):
    
    # convert symbolic expressions using numpy
    lam_x = sp.lambdify(x_data,y_data , modules=['numpy'])
    x_vals = linspace((x_min), (x_max), 100)
    y_vals = lam_x(x_vals) 
    
    fig = mpl.figure(figsize=(10,6))
    ax = mpl.axes()
    ax.grid(True,which='major',axis='both',alpha=0.3) 
    mpl.plot(x_vals, y_vals)
    #mpl.show(fig)
    
    return

# needs to get added to helper
def GetInputVariables(string):
    #opening the file that has the inputs to the FORTRAN Code
    InputFile = open("../../FortranFiles/main-scripts/main-variables.f90","r")
    
    flag  = 0
    index = 0
    
    for line in InputFile:
        index += 1
        
        # checking if the string is present in line or not
        if string in line:
            result = line
            flag = 1  
            break
    # checking condition for string found or not, uncomment else when debugging
    if flag == 0:
        print('String', string, 'Not Found')
    #else:
        #print('String', string, 'Found In Line', index)
        
    # using re.findall()
    # getting numbers from string 
    result = re.findall(r'\d*\.?\d+', result)
    
    return result

def plotSymbolicEquation(title  , \
                         x_data , \
                         x_title, \
                         y_data , \
                         y_title, \
                         x_min  , \
                         x_max):
    
    # convert symbolic expressions using numpy
    lam_x  = sp.lambdify(x_data,y_data , modules=['numpy'])
    x_vals = linspace((x_min), (x_max), 100)
    y_vals = lam_x(x_vals) 
    
    fig = mpl.figure(figsize=(10,6))
    ax = mpl.axes()
    
    ax.grid(True,which='major',axis='both',alpha=0.3) 
    mpl.title(title)
    mpl.xlabel(x_title)
    mpl.ylabel(y_title)
    mpl.plot(x_vals, y_vals)
    mpl.show(fig)
    
    return 

def SourceSubstitution(S, \
                       A_analytic         , \
                       M_t_analytic       , \
                       M_x_analytic     , \
                       v_r_analytic     , \
                       v_t_analytic     , \
                       v_x_analytic     , \
                       p_analytic       , \
                       dp_dr_analytic   , \
                       dv_r_dr_analytic , \
                       dM_x_dr_analytic , \
                       dM_t_dr_analytic , \
                      ): 
    
    #print('Input Source: ', S)
    S = S.subs({ \
            A:A_analytic       , \
            M_t:M_t_analytic   , \
            M_x:M_x_analytic , \
            v_r:v_r_analytic , \
            v_t:v_t_analytic , \
            v_x:v_x_analytic , \
            p:p_analytic     , \
            dp_dr:dp_dr_analytic, \
            dv_r_dr:dv_r_dr_analytic, \
            dM_x_dr:dM_x_dr_analytic, \
            dM_t_dr:dM_t_dr_analytic, \
               })
    #print('Output Source', S)
    return S