#!/usr/bin/env python
# coding: utf-8

# In[1]:


import numpy as np
import scipy as scip
from scipy import special
import cmath
import matplotlib.pyplot as plt


# In[64]:


# Compute zeros of integer-order Bessel function derivatives Jn'.
bessel_order = 10 
num_of_zeros = 10
Jv_zero = scip.special.jnp_zeros(n  = bessel_order, nt = num_of_zeros)
#Jv      = scip.special.jv(10,Jv_zero)
x_min   = 0
x_max   = 20
x_steps = 100
x       = np.linspace(x_min,x_max,x_steps)

Jv      = scip.special.jv(bessel_order,x)
print(Jv_zero)
plt.plot(x,Jv)
plt.plot(Jv_zero[0],0,marker='o')
plt.plot(Jv_zero[1],0,marker='o')

plt.savefig('tex-outputs/bessel_mode.pdf', 
        format = 'pdf', 
        bbox_inches='tight')


# In[ ]:




