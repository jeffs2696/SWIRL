#!/usr/bin/python3
import numpy as np
import cmath
import scipy as scp
import matplotlib as mpl
import matplotlib.pyplot as plt
import math
import pandas
from math import radians, cos, sin, asin, sqrt

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
