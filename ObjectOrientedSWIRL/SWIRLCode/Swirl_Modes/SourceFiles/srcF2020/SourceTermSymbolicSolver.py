#!/usr/bin/env python
# coding: utf-8

# In[253]:


# Importing libraries
from sympy import *
from sympy.plotting import plot
from sympy.utilities.codegen import codegen
from sympy.utilities.autowrap import autowrap
from sympy.interactive import printing
from numpy import linspace
printing.init_printing(use_latex = True) 
import sympy as sp
import numpy as np
import math
import re
import numpy as np
import matplotlib.pyplot as plt

# Defining symbolic variables needed for this code.
# all units are dimensionless! 

# radius, maximum radius, and ratio of specific heats
r, r_max, kappa = symbols('r r_max kappa')

# arbitrary constants
k = symbols('k', cls=IndexedBase)
one, two, three = symbols('one two three')
C = symbols('C')
x = symbols('x')

# reduced frequency
ak = symbols('ak')

# imaginary #, Speed of sound, azimuthal mode number, axial wavenumber 
i, A, m, gamma   = symbols('i A m gamma')
v_r, v_t, v_x, p = symbols('v_r v_t v_x p ')
M_t, M_x         = symbols('M_t M_x')
dp_dr, dv_r_dr   = symbols('dp_dr dv_r_dr')
dM_x_dr, dM_t_dr = symbols('dM_x_dr dM_t_dr')


# In[254]:


lamm = one - k[1]*sp.tanh(k[2]*(one-r_max))

A_analytic        = lamm + k[1]*sp.tanh(k[2]*(r-r_max))

dA_analytic_dr    = diff(A_analytic,r)
dA_analytic_sq_dr = diff(A_analytic**two,r)
#pprint(simplify(dA_analytic_dr))

M_t_analytic = simplify(sp.sqrt(                        r/((kappa-one)*A_analytic**two) *                        (dA_analytic_sq_dr)))

#print(latex(simplify(M_t_analytic)))


# In[255]:


# Using the expression for the tangential Mach number, M_t above ...
f_A   = fcode(A_analytic  ,source_format='free',standard=95)
f_M_t = fcode(M_t_analytic,source_format='free',standard=95)
#print(f_A)
#print(f_M_t)
f_M_t = re.sub(r"\*\*2","**2.0_rDef",f_M_t)

#print(f_A)
#print(f_M_t)


# In[256]:


f_A   = "        SoundSpeedExpected(i) = " + f_A +"\n"
f_M_t = "        thetaMachData(i)      = " + f_M_t + "\n"

f_A = re.sub(r"r ","r(i)",f_A)
f_A = re.sub(r"r\*","r(i)*",f_A)
f_A = re.sub(r"k\(","kR(",f_A)
f_A = re.sub(r"k\(1\)","kR(1)",f_A)
f_A = re.sub(r"k\(2\)","kR(2)",f_A)

f_M_t = re.sub(r"r ","r(i)",f_M_t)
f_M_t = re.sub(r"r\*","r(i)*",f_M_t)
f_M_t = re.sub(r"k\(1\)","kR(1)",f_M_t)
f_M_t = re.sub(r"k\(2\)","kR(2)",f_M_t)
f_M_t = re.sub(r"k\(","kR(",f_M_t)

S_list = []
S_list.append(f_A)
S_list.append(f_M_t)
S_list = ''.join(S_list)


# In[257]:



f_code_header1 = ''' 
    SUBROUTINE CalcSoundSpeed(& 
    r                 , &
    r_max             , &
    k, kappa          , &
    SoundSpeedExpected, &
    thetaMachData       &
    )
    
    REAL(KIND=rDef)   , INTENT(IN) :: &
    r_max,kappa
    
    REAL(KIND=rDef)   , DIMENSION(:), INTENT(INOUT) :: &
    SoundSpeedExpected, thetaMachData
    
    REAL(KIND=rDef)   , DIMENSION(:), INTENT(IN) :: &
    r
    
    COMPLEX(KIND=rDef), DIMENSION(:), INTENT(IN) :: &
    k
     
    ! Local variables 
    INTEGER :: &
    numberOfGridPoints, i
    
    REAL(KIND = rDef) :: one,two,three
    
    REAL(KIND=rDef), DIMENSION(SIZE(k)) :: kR
    
    kR = REAL(k,KIND = rDef)
        
    one   = (1.0_rDef)    
    two   = (2.0_rDef)    
    three = (3.0_rDef)

    
    numberOfGridPoints = SIZE(SoundSpeedExpected)

        DO i = 1,numberOfGridPoints

'''    

f_code_footer1 = '''

        END DO

    END SUBROUTINE CalcSoundSpeed
'''

with open('SoundSpeedMMS.f90','w') as f:
    f.write(f_code_header1)
    f.write(S_list)
    f.write(f_code_footer1)

    
#with open('SoundSpeedMMS.f90','r') as f:
#    for line in f:
#        print(line)


# In[258]:


S_1 = i*(-ak/A + (m/r)*M_t - gamma*M_x)*v_r + (two/r)*M_t*v_t - dp_dr - ((kappa - one)/(two*r))*M_t**two*p

S_2 = i*(-ak/A + (m/r)*M_t - gamma*M_x)*v_t + (M_t/r - dM_t_dr - ((kappa - one)/(two*r))*M_t**three)*v_r + i*m*p/r

S_3 = i*(-ak/A + (m/r)*M_t - gamma*M_x)*v_x + (dM_x_dr - ((kappa - one)/(two*r))*M_x*M_t**two)*v_r + i*gamma*p

S_4 = i*(-ak/A + (m/r)*M_t - gamma*M_x)*p   + dv_r_dr + (((kappa - one)/(two*r))*M_t**two + one/r)*v_r + i*m*v_t/r + i*gamma*v_x

# Lets look at the source terms
#pprint('The linearized (unsteady) Euler Equations used in SWIRL:')
#pprint(('S_1=',S_1))
#pprint(('S_2=',S_2))
#pprint(('S_3=',S_3))
#pprint(('S_4=',S_4))


# In[259]:


S_1 = (S_1.subs({A:A_analytic, M_t:M_t_analytic}))
S_2 = (S_2.subs({A:A_analytic, M_t:M_t_analytic}))
S_3 = (S_3.subs({A:A_analytic, M_t:M_t_analytic}))
S_4 = (S_4.subs({A:A_analytic, M_t:M_t_analytic}))


# In[260]:


M_x_analytical = sp.cos(k[2]*(r - r_max))
v_r_analytical = sp.cos(k[3]*(r - r_max))
v_t_analytical = sp.cos(k[4]*(r - r_max))
v_x_analytical = sp.cos(k[5]*(r - r_max))
p_analytical   = sp.cos(k[6]*(r - r_max))

S_1 = (S_1.subs({M_x:M_x_analytical,                  v_r:v_r_analytical,                  v_t:v_t_analytical,                  v_x:v_x_analytical,                  p:p_analytical,                 }))

S_2 = (S_2.subs({M_x:M_x_analytical,                  v_r:v_r_analytical,                  v_t:v_t_analytical,                  v_x:v_x_analytical,                  p:p_analytical,                 }))

S_3 = (S_3.subs({M_x:M_x_analytical,                  v_r:v_r_analytical,                  v_t:v_t_analytical,                  v_x:v_x_analytical,                  p:p_analytical,                 }))

S_4 = (S_4.subs({M_x:M_x_analytical,                  v_r:v_r_analytical,                  v_t:v_t_analytical,                  v_x:v_x_analytical,                  p:p_analytical,                 }))


# In[261]:


dp_dr_analytical   = p_analytical.diff(r)
dv_r_dr_analytical = v_r_analytical.diff(r) 
dM_x_dr_analytical = M_x_analytical.diff(r)
dM_t_dr_analytical = M_t_analytic.diff(r)


S_1 = (S_1.subs({                 dp_dr:dp_dr_analytical,                 dv_r_dr:dv_r_dr_analytical,                 dM_x_dr:dM_x_dr_analytical,                 dM_t_dr:dM_t_dr_analytical,                }))

S_2 = (S_2.subs({                 dp_dr:dp_dr_analytical,                 dv_r_dr:dv_r_dr_analytical,                 dM_x_dr:dM_x_dr_analytical,                 dM_t_dr:dM_t_dr_analytical,                })) 

S_3 = (S_3.subs({                 dp_dr:dp_dr_analytical,                 dv_r_dr:dv_r_dr_analytical,                 dM_x_dr:dM_x_dr_analytical,                 dM_t_dr:dM_t_dr_analytical,                }))

S_4 = (S_4.subs({                 dp_dr:dp_dr_analytical,                 dv_r_dr:dv_r_dr_analytical,                 dM_x_dr:dM_x_dr_analytical,                 dM_t_dr:dM_t_dr_analytical,                }))


# In[262]:


fS_1 = fcode(S_1,source_format='free',standard=95)
fS_2 = fcode(S_2,source_format='free',standard=95)
fS_3 = fcode(S_3,source_format='free',standard=95)
fS_4 = fcode(S_4,source_format='free',standard=95)

fS_1 = "    S_1 = " + re.sub(r"gamma",'gam',fS_1) + "\n"
fS_2 = "    S_2 = " + re.sub(r"gamma",'gam',fS_2) + "\n"
fS_3 = "    S_3 = " + re.sub(r"gamma",'gam',fS_3) + "\n"
fS_4 = "    S_4 = " + re.sub(r"gamma",'gam',fS_4) + "\n"

fS_1 = re.sub(r"\*m\*","*mC*",fS_1)
fS_2 = re.sub(r"\*m\*","*mC*",fS_2)
fS_3 = re.sub(r"\*m\*","*mC*",fS_3)
fS_4 = re.sub(r"\*m\*","*mC*",fS_4)

fS_1 = re.sub(r"kappa","kappaC",fS_1)
fS_2 = re.sub(r"kappa","kappaC",fS_2)
fS_3 = re.sub(r"kappa","kappaC",fS_3)
fS_4 = re.sub(r"kappa","kappaC",fS_4)

fS_1 = re.sub(r"r_max","r_maxC ",fS_1)
fS_2 = re.sub(r"r_max","r_maxC ",fS_2)
fS_3 = re.sub(r"r_max","r_maxC ",fS_3)
fS_4 = re.sub(r"r_max","r_maxC ",fS_4)


# In[263]:


S_list = []
S_list.append(fS_1)
S_list.append(fS_2)
S_list.append(fS_3)
S_list.append(fS_4)
S_list = ''.join(S_list)


f_code_header2 = ''' 
! gam - axial wavenumber 
! ak  - reduced frequency
! kappa - ratio of specific heats
! i - imaginary number

    SUBROUTINE SourceCalc(& 
    gam  , &
    i    , &
    ak   , &
    k    , &
    kappa, &
    m    , & 
    r    , &
    r_max, &
    S_1  , &
    S_2  , &
    S_3  , &
    S_4)
    
    INTEGER, INTENT(IN) :: m
    REAL(KIND=rDef)   , INTENT(IN) :: kappa,r,r_max 
    COMPLEX(KIND=rDef), INTENT(IN) :: i, gam, ak           
    COMPLEX(KIND=rDef), INTENT(INOUT) :: S_1, S_2, S_3, S_4
    COMPLEX(KIND=rDef), DIMENSION(:), INTENT(IN) :: k
    
    ! Local variables
    COMPLEX(KIND=rDef) :: mC, kappaC, rC, r_maxC
    
    COMPLEX(KIND=rDef) :: one,two,three
    
    one = CMPLX(1.0,KIND=rDef)    
    two = CMPLX(2.0,KIND=rDef)    
    three = CMPLX(3.0,KIND=rDef)
    
    mC = CMPLX(m,KIND=rDef)
    kappaC = CMPLX(kappa,KIND=rDef)
    rC = CMPLX(r,KIND=rDef)
    r_maxC = CMPLX(r_max,KIND=rDef)
'''

f_code_footer2 = '''
    END SUBROUTINE SourceCalc
'''
#print(fS_1)
#print(fS_2)
#print(fS_3)
#print(fS_4)

#print(S_1)
#print(S_2)
#print(S_3)
#print(S_4)


with open('SourceTermMMS.f90','w') as f:
    f.write(f_code_header2)
    f.write(S_list)
    f.write(f_code_footer2)

