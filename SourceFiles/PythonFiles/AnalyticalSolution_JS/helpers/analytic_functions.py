#!/usr/bin/python3
import numpy as np
import scipy as scp
import cmath
def get_radial_wavenumbers(
        azimuthal_mode_number,
        num_of_zeros): 

    Jv_p_zero = scp.special.jnp_zeros(
            n  = azimuthal_mode_number,
            nt = num_of_zeros)

    return Jv_p_zero
def get_radial_modes(
        azimuthal_mode_number,
        Jv_p_zero,
        r):

    radial_mode_list = []
    for i in range(len(Jv_p_zero)): 
        radial_mode = scp.special.jv(
                azimuthal_mode_number,
                abs(Jv_p_zero[i])*r)
        radial_mode_list.append(radial_mode)

    return radial_mode_list
def get_axial_wavenumbers(azimuthal_mode_number,k,M_x,Jv_p_zero):

    pm = np.array([1,-1])
    k_x_plus = np.array((Jv_p_zero),dtype=object) 
    k_x_minus = np.array((Jv_p_zero),dtype=object) 
    for i,j in enumerate(Jv_p_zero): 


        k_x_plus[i] = (((-M_x*k +  pm*cmath.sqrt(k**2 - (1-M_x**2)*Jv_p_zero[i]**2))/(1-M_x**2))) [0]
        k_x_minus[i] = (((-M_x*k +  pm*cmath.sqrt(k**2 - (1-M_x**2)*Jv_p_zero[i]**2))/(1-M_x**2))) [1]

    if azimuthal_mode_number < 1:
        # append wavenumber (frequency) as the first wavenumber when m = 0 
        k_x_plus = np.append(k,k_x_plus)
        k_x_minus = np.append(-k,k_x_minus)

    return k_x_plus,k_x_minus

def normalize_psi(psi, x):

    if type(psi) is list:
        normalization_constant = []
        for psi_i in range(len(psi)):
            int_psi_square = np.trapz(abs(psi_i)*abs(np.conj((psi_i)))*x, x)
            normalization_constant.append(np.sqrt(1/int_psi_square))
    else:
        # note this is for cylindrical only hence the extra x
        int_psi_square = np.trapz(abs(psi)*abs(np.conj((psi)))*x, x)
        normalization_constant = np.sqrt(1/int_psi_square)

    return normalization_constant

