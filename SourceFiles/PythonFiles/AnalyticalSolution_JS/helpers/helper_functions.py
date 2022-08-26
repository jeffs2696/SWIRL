import numpy as np
import cmath
import scipy as scp
import matplotlib as mpl
import matplotlib.pyplot as plt

def set_plot_parameters():
    plt.style.use('fivethirtyeight')
    width = 345
    tex_fonts = {
            # Use LaTeX to write all text
            "text.usetex": True,
            "font.family": "serif",
            # Use 10pt font in plots, to match 10pt font in document
            "axes.labelsize": 10,
            "font.size": 10,
            # Make the legend/label fonts a little smaller
            "legend.fontsize": 8,
            "xtick.labelsize": 8,
            "ytick.labelsize": 8
            } 
    mpl.rcParams.update(tex_fonts) 

    return

def axial_wavenumber_quadratic(k,M_x,Jv_p_zero):
    pm = np.array([1,-1])
    k_x_plus = np.array((Jv_p_zero),dtype=object) 
    k_x_minus = np.array((Jv_p_zero),dtype=object) 
    for i,j in enumerate(Jv_p_zero): 


        k_x_plus[i] = (((-M_x*k +  pm*cmath.sqrt(k**2 - (1-M_x**2)*Jv_p_zero[i]**2))/(1-M_x**2))) [0]
        k_x_minus[i] = (((-M_x*k +  pm*cmath.sqrt(k**2 - (1-M_x**2)*Jv_p_zero[i]**2))/(1-M_x**2))) [1]

    return k_x_plus,k_x_minus

def normalize_psi(psi, x):

    int_psi_square = np.trapz(abs(psi)*abs(np.conj(psi))*x, x)

    normalization_constant = np.sqrt(1/int_psi_square)

    return normalization_constant

def set_size(width, fraction=2):
    """Set figure dimensions to avoid scaling in LaTeX.  
    Parameters
    ----------
    width: float
    Document textwidth or columnwidth in pts
    fraction: float, optional
    Fraction of the width which you wish the figure to occupy 

    Returns
    -------
    fig_dim: tuple
    Dimensions of figure in inches
    """
    # Width of figure (in pts)
    fig_width_pt = width * fraction 
    # Convert from pt to inches
    inches_per_pt = 1 / 72.27 
    # Golden ratio to set aesthetic figure height
    # https://disq.us/p/2940ij3
    golden_ratio = (5**.5 - 1) / 2
    # Figure width in inches
    fig_width_in = fig_width_pt * inches_per_pt
    # Figure height in inches
    fig_height_in = fig_width_in * golden_ratio

    fig_dim = (fig_width_in, fig_height_in) 
    return fig_dim
