# ========= Packages ========
import sympy as sp 
import numpy as np
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
    max_amplitude = 0.25
   
    # vertical shift along the y axis
    # one is chosed to keep the inflection points above 
    # zero
    S_vertical = 1
    
    # rj is the list of inflection point locations
    rj = list(np.linspace(1,0,n))


    
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
    print(A)
    return f

