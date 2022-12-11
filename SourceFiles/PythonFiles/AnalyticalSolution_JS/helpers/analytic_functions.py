#!/usr/bin/python3
import numpy as np
import scipy as scp
import cmath
def get_radial_wavenumbers(
        azimuthal_mode_number,
        num_of_zeros): 
    """ 
    """

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
                (Jv_p_zero[i])*r)
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
            int_psi_square = np.trapz((psi_i)*(np.conj((psi_i)))*x, x)
            normalization_constant.append(np.sqrt(1/int_psi_square))
    else:
        # note this is for cylindrical only hence the extra x
        int_psi_square = np.trapz((psi)*(np.conj((psi)))*x, x)
        normalization_constant = np.sqrt(1/int_psi_square)

    return normalization_constant

def normalize_radial_modes(r,radial_mode_list,radial_mode_number,azimuthal_mode_number,Jv_p_zero):
    """
    Objective
    ---------
    
    Parameters
    -----------
    r : numpy array 

    radial_mode_list : nested list containing numpy arrays of analytic mode data
    (see output for get_radial_modes)
    radial_mode_number : integer for the corresponding radial mode number  


    """

    # print('entering normalize_radial_modes')
    normalized_radial_mode_list = []
    analytical_normalized_radial_mode_list = []

    for i,j in enumerate(radial_mode_list):
        radial_mode = radial_mode_list[i]
        radial_mode_number_for_list = i
        
        normalization_constant = normalize_psi((radial_mode),r)

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
    # print(normalized_radial_mode_list)
    # print('leaving normalize_radial_modes')
    return normalized_radial_mode_list,analytical_normalized_radial_mode_list

def get_multiple_radial_modes(grid_point_array,r_min,r_max):

    for i_gp,j_gp in enumerate(grid_point_array): 
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
        # This depend on whether you count the first 0 at the boundary or not.. 
        normalized_radial_mode_list, \
                analytical_normalized_radial_mode_list = \
                afcn.normalize_radial_modes(
                        r,
                        radial_mode_list,
                        radial_mode_number,
                        azimuthal_mode_number,
                        Jv_p_zero) 
    return
