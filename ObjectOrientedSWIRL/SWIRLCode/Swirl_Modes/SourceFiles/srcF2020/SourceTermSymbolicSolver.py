#!/usr/bin/env python
# coding: utf-8

# The goal of this python script is to generate source terms for the simplified linearized Euler equations. This script was writted in jupyter notebook.
# 
# The linearized Euler equations after substitUting the exponential form for the pertubation quantities $\bar{v}_r \bar{v}_{\theta} \bar{v}_x \bar{p}$
# 
# $$ i \left( \frac{k}{\bar{A}} - \frac{m}{\bar{r}} - \bar{\gamma}{M_x} \right) \bar{v}_r + \frac{2}{\bar{r}}M_{\theta}\bar{v_{\theta}} -\frac{d \bar{p}}{d\bar{r}} - \frac{\kappa - 1}{\bar{r}} M_{\theta}^2 \bar{p} = S_1$$
# 
# $$ -i \left( 
# \frac{k}{\bar{A}} -
# \frac{m}{\bar{r}} - 
# \bar{\gamma}{M_x} 
# \right) \bar{v}_{\theta} + 
# \left[ 
# \frac{M_{\theta}}{\bar{r}} + \frac{d M_{\theta}}{d \bar{r}} + \frac{\kappa - 1}{2 \bar{r}} M_{\theta}^3\right] \bar{v}_r + \frac{im}{\bar{r}}\bar{p}  = S_2$$
# 
# $$ -i \left( 
# \frac{k}{\bar{A}} -
# \frac{m}{\bar{r}} - 
# \bar{\gamma}{M_x} 
# \right) \bar{v}_{x} + 
# \left[ 
# \frac{d M_{x}}{d \bar{r}} +
# \frac{\kappa - 1}{2 \bar{r}} M_x M_{\theta}^2\right] \bar{v}_r + i\bar{\gamma}{\bar{p}} = S_3  $$
# 
# 
# $$ -i \left( 
# \frac{k}{\bar{A}} -
# \frac{m}{\bar{r}} - 
# \bar{\gamma}{M_x} 
# \right) \bar{p} + 
# \frac{d \bar{v}_r}{d \bar{r}} +
# \left[ 
# \frac{\kappa - 1}{2 \bar{r}} M_{\theta}^2 +
# \frac{1}{\bar{r}}
# \right] \bar{v}_r + 
# \frac{im}{\bar{r}}\bar{v}_{\theta} +
# i \bar{\gamma} \bar{v}_x = S_4$$

# First we will go over the integration process used to obtain the speed of sound $A$ from $M_{\theta}$
# 
# $$\bar{A}(\bar{r}) =  \exp 
# \left[ 
# \frac{1-\kappa}{2}\int_{\bar{r}}^{\bar{r}_{max}}\frac{M_\theta^2}{\bar{r}} dr 
# \right]$$ 
# 
# Defining an analytical expression for $M_{\theta}$ from this equation yields,
# 
# $$ M_{\theta} = \sqrt{  
# \frac{\bar{r}}{(\kappa - 1) \bar{A}} 
# \frac{\partial \bar{A}^2}{\partial \bar{r}}
# }$$
# 
# The input for the speed of sound is defined as $\bar{A}_{analytic}$, however this is only an analytical function that has been chosen but does not provide a physical and meaningful solution to the problem on its own. For simplicity,
# 
# $$ \bar{A}_{analytic} = \cos \left( k (\bar{r} - \bar{r}_{max} )\right) $$
# And the derivative with respect to non dimensional radius is,
# 
# $$ \frac{\partial \bar{A}_{analytic}}{\partial \bar{r} } = 
# \frac{\partial}{\partial \bar {r}} \left( 
# \cos \left( k (\bar{r} - \bar{r}_{max} )\right) 
# \right)$$
# 
# $$ \frac{\partial \bar{A}_{analytic}}{\partial \bar{r} } = 
# -k \sin \left( k ( \bar{r} - \bar{r}_{max}) \right)
# $$
# 
# Plugging this into the expression for $M_{\theta}$
# \begin{align*}                                                                                                                                                                                                                                    M_{\theta} = \sqrt{2}\,\sqrt{-\frac{k\,r\,\sin\left(k\,\left(r-r_{\mathrm{max}}\right)\right)}{(\gamma - 1)\,\cos\left(k\,\left(r-r_{\mathrm{max}}\right)\right)}}                                                                        \end{align*}           
# 
# However, this gives a negative number in the square root which adds complexity to the fortran output, particularly at the boundaries, instead we will use the sin function,
# 
# $$ \bar{A}_{analytic} = \sin \left( k \frac{\bar{r}}{\bar{r}_{max}} )\right) $$
# And the derivative with respect to non dimensional radius is,
# 
# $$ \frac{\partial \bar{A}_{analytic}}{\partial \bar{r} } = 
# \frac{\partial}{\partial \bar {r}} \left( 
# \sin \left( k (\bar{r} - \bar{r}_{max} )\right) 
# \right)$$
# 
# $$ \frac{\partial \bar{A}_{analytic}}{\partial \bar{r} } = 
# k \sin \left( k ( \bar{r} - \bar{r}_{max}) \right)
# $$
# 

# In[1]:


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


# In[2]:


# Defining symbolic variables needed for this code.
# all units are dimensionless! 

# radius, maximum radius, and ratio of specific heats
r, r_max, kappa = symbols('r r_max kappa')

# arbitrary constants
k = symbols('k', cls=IndexedBase)
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


# Defining an arbitrary analytical function for the speed of sound
# as well as taking its derivative and the derivative of the square
# of the speed of sound ...

# In[3]:


A_analytic        = (sp.exp(k[1]*(r-r_max)))

#sp.exp(-k[1]*(r**(-2.0)-r_max))/sp.exp(-k[2]*(r**(2.0)-r_max))
dA_analytic_dr    = diff(A_analytic,r)
dA_analytic_sq_dr = diff(A_analytic**2.0,r)

M_t_analytic = sp.sqrt(                        r/((kappa-1.0)*A_analytic**2.0) *                        (dA_analytic_sq_dr))
print(M_t_analytic)


# In[4]:


# Using the expression for the tangential Mach number, M_t above ...
f_A   = fcode(A_analytic  ,source_format='free',standard=95)
f_M_t = fcode(M_t_analytic,source_format='free',standard=95)

f_A   = "        SoundSpeedExpected(i) = " + f_A +"\n"
f_M_t = "        thetaMachData(i)      = " + f_M_t + "\n"

f_A = re.sub(r"r ","r(i)",f_A)
f_A = re.sub(r"r\*","r(i)*",f_A)

f_M_t = re.sub(r"r ","r(i)",f_M_t)
f_M_t = re.sub(r"r\*","r(i)*",f_M_t)

S_list = []
S_list.append(f_A)
S_list.append(f_M_t)
S_list = ''.join(S_list)

f_code_header1 = ''' 
    SUBROUTINE CalcSoundSpeed(& 
    r  , &
    r_max , &
    k, kappa, SoundSpeedExpected,thetaMachData       &
    )
    
    REAL(KIND=rDef)   , INTENT(IN) :: &
    r_max,kappa
    
    REAL(KIND=rDef), DIMENSION(:), INTENT(INOUT) :: &
    SoundSpeedExpected, thetaMachData
    
    REAL(KIND=rDef), DIMENSION(:), INTENT(IN) :: r
    
    COMPLEX(KIND=rDef), DIMENSION(:), INTENT(IN) :: k
     
    INTEGER :: numberOfGridPoints, i
    
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

    
with open('SoundSpeedMMS.f90','r') as f:
    for line in f:
        print(line)


# In[5]:


S_1 = i*(-ak/A + (m/r)*M_t - gamma*M_x)*v_r + (2.0/r)*M_t*v_t - dp_dr - ((kappa - 1.0)/(2.0*r))*M_t**2.0*p

S_2 = i*(-ak/A + (m/r)*M_t - gamma*M_x)*v_t + (M_t/r - dM_t_dr - ((kappa - 1.0)/2.0*r)*M_t**3.0)*v_r + i*m*p/r

S_3 = i*(-ak/A + (m/r)*M_t - gamma*M_x)*v_x + (dM_x_dr - ((kappa - 1.0)/(2.0*r))*M_x*M_t**2.0)*v_r + i*gamma*p

S_4 = i*(-ak/A + (m/r)*M_t - gamma*M_x)*p   + dv_r_dr + (((kappa - 1.0)/(2.0*r))*M_t**2.0 + 1.0/r)*v_r + i*m*v_t/r + i*gamma*v_x

# Lets look at the source terms
#pprint('The linearized (unsteady) Euler Equations used in SWIRL:')
#pprint(('S_1=',S_1))
#pprint(('S_2=',S_2))
#pprint(('S_3=',S_3))
#pprint(('S_4=',S_4))


# Now Lets make the mean flow substitutions from the Speed of Sound.
# We still need to define an analytical axial Mach number as well as 
# functions for the perturbation variables

# In[6]:


S_1 = (S_1.subs({A:A_analytic, M_t:M_t_analytic}))
S_2 = (S_2.subs({A:A_analytic, M_t:M_t_analytic}))
S_3 = (S_3.subs({A:A_analytic, M_t:M_t_analytic}))
S_4 = (S_4.subs({A:A_analytic, M_t:M_t_analytic}))


# In[7]:


M_x_analytical = sp.cos(k[2]*(r - r_max))
v_r_analytical = sp.cos(k[3]*(r - r_max))
v_t_analytical = sp.cos(k[4]*(r - r_max))
v_x_analytical = sp.cos(k[5]*(r - r_max))
p_analytical   = sp.cos(k[6]*(r - r_max))

S_1 = (S_1.subs({M_x:M_x_analytical,                  v_r:v_r_analytical,                  v_t:v_t_analytical,                  v_x:v_x_analytical,                  p:p_analytical,                 }))

S_2 = (S_2.subs({M_x:M_x_analytical,                  v_r:v_r_analytical,                  v_t:v_t_analytical,                  v_x:v_x_analytical,                  p:p_analytical,                 }))

S_3 = (S_3.subs({M_x:M_x_analytical,                  v_r:v_r_analytical,                  v_t:v_t_analytical,                  v_x:v_x_analytical,                  p:p_analytical,                 }))

S_4 = (S_4.subs({M_x:M_x_analytical,                  v_r:v_r_analytical,                  v_t:v_t_analytical,                  v_x:v_x_analytical,                  p:p_analytical,                 }))


# Note that there is still derivative terms in each of the source terms, let's evaluate those derivatives
# and substitute those in

# In[8]:


dp_dr_analytical   = p_analytical.diff(r)
dv_r_dr_analytical = v_r_analytical.diff(r) 
dM_x_dr_analytical = M_x_analytical.diff(r)
dM_t_dr_analytical = M_t_analytic.diff(r)


S_1 = (S_1.subs({                 dp_dr:dp_dr_analytical,                 dv_r_dr:dv_r_dr_analytical,                 dM_x_dr:dM_x_dr_analytical,                 dM_t_dr:dM_t_dr_analytical,                }))

S_2 = (S_2.subs({                 dp_dr:dp_dr_analytical,                 dv_r_dr:dv_r_dr_analytical,                 dM_x_dr:dM_x_dr_analytical,                 dM_t_dr:dM_t_dr_analytical,                })) 

S_3 = (S_3.subs({                 dp_dr:dp_dr_analytical,                 dv_r_dr:dv_r_dr_analytical,                 dM_x_dr:dM_x_dr_analytical,                 dM_t_dr:dM_t_dr_analytical,                }))

S_4 = (S_4.subs({                 dp_dr:dp_dr_analytical,                 dv_r_dr:dv_r_dr_analytical,                 dM_x_dr:dM_x_dr_analytical,                 dM_t_dr:dM_t_dr_analytical,                }))


# Now that the symbolic expressions are solved for, we can write a FORTRAN Code!
# Note that there are many ways to do this, see: https://docs.sympy.org/latest/modules/codegen.html

# In[9]:


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


# In[10]:


#fS_1 = re.sub(r"r ","rC ",fS_1)
#fS_2 = re.sub(r"r ","rC ",fS_2)
#fS_3 = re.sub(r"r ","rC ",fS_3)
#fS_4 = re.sub(r"r ","rC ",fS_4)

#fS_1 = re.sub(r"-r","-rC",fS_1)
#fS_2 = re.sub(r"-r","-rC",fS_2)
#fS_3 = re.sub(r"-r","-rC",fS_3)
#fS_4 = re.sub(r"-r","-rC",fS_4)


#fS_1 = re.sub(r"\(r\*","(rC*",fS_1)
#fS_1 = re.sub(r"\/r\)","/rC\) ",fS_1)
#fS_1 = re.sub(r"\/r","/rC",fS_1)
#fS_1 = re.sub(r"\/rCC","/rC",fS_1)
#fS_1 = re.sub(r"\*r\*","*rC*",fS_1)
#fS_1 = re.sub(r"\(r\*\*","(rC** ",fS_1)

#print(fS_1)
#fS_2 = re.sub(r"\(r\*","(rC*",fS_2)
#fS_1 = re.sub(r"\/r\)","/rC\) ",fS_1)
#fS_2 = re.sub(r"\/r","/rC",fS_2)
#fS_2 = re.sub(r"\/rCC","/rC",fS_2)
#fS_2 = re.sub(r"\*r\*","*rC*",fS_2)
#fS_2 = re.sub(r"\(r\*\*","(rC** ",fS_2)

#fS_3 = re.sub(r"\(r\*","(rC* ",fS_3)

#fS_3 = re.sub(r"\/r","/rC",fS_3)
#fS_3 = re.sub(r"\/rCC","/rC",fS_3)
#fS_3 = re.sub(r"\*r\*","*rC*",fS_3)
#fS_3 = re.sub(r"\(r\*\*","(rC** "89,fS_3)
#fS_3 = re.sub(r"\(r\* \*","(rC** ",fS_3)

#fS_4 = re.sub(r"\(r\*","(rC* ",fS_4)
#fS_4 = re.sub(r"\(r\*\*","(rC** ",fS_4)
#fS_1 = re.sub(r"\/r\)","/rC\) ",fS_1)
#fS_4 = re.sub(r"\/r","/rC",fS_4)
#fS_4 = re.sub(r"\/rCC","/rC",fS_4)
#fS_4 = re.sub(r"\*r\*","*rC*",fS_4)
#fS_4 = re.sub(r"\(r\*\*","(rC** ",fS_4)


# In[11]:


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
    
    mC = CMPLX(m,KIND=rDef)
    kappaC = CMPLX(kappa,KIND=rDef)
    rC = CMPLX(r,KIND=rDef)
    r_maxC = CMPLX(r_max,KIND=rDef)
'''

f_code_footer2 = '''
    END SUBROUTINE SourceCalc
'''
print(fS_1)
print(fS_2)
print(fS_3)
print(fS_4)

print(S_1)
print(S_2)
print(S_3)
print(S_4)


with open('SourceTermMMS.f90','w') as f:
    f.write(f_code_header2)
    f.write(S_list)
    f.write(f_code_footer2)


# 
