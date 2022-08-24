#!/usr/bin/python3
from cycler import cycler
import math 
import cmath 
import matplotlib.pyplot as plt
import numpy as np

def set_size(width, fraction=1):
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
# plot parameters
# Create cycler object. Use any styling from above you please
monochrome = (
    cycler(
        'color',
        ['royalblue', 'orangered', 'goldenrod', 'black']
    ) +
    cycler('linestyle', ['-', '--', ':', '-.']
           )  # + cycler( 'marker', ['.','^',',', '.']) + cycler( 'markevery',[125,150,175,100])
)

markers = ['o-', '+-', '--', '-', 'o-', '.', 'x', 'X', 'D', '|']
width = 400 

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

# plt.rcParams['figure.constrained_layout.use'] = True
plt.rcParams.update(tex_fonts)
plt.rcParams['axes.grid'] = True
plt.rcParams['axes.prop_cycle'] = monochrome
plt.rcParams['axes.spines.top'] = False
plt.rcParams['axes.spines.right'] = False
plt.rcParams['axes.spines.bottom'] = False
plt.rcParams['axes.spines.left'] = False

start = 0
stop  = 1 
step  = 0.01 

x = np.arange(start,stop,step)
axial_wavenumber = 10

desired_amplitude_for_cut_off = 0.01
axial_location_of_cut_off = -math.log(0.01)/axial_wavenumber 

fluctuation = np.exp(-axial_wavenumber*x)

# Fig. 1:
fig, ax = plt.subplots(
    nrows=1,
    ncols=1,
    sharex=True,
    figsize=set_size(width),
)

title_txt = r'Decaying Mode Example $y = \exp(${wavenum:.0f}$x$)'
fig.suptitle(title_txt.format(wavenum= axial_wavenumber))
ax.semilogy(
        x,
        fluctuation 
)
ax.annotate(
        r'1 \% of the maximum amplitude',
        xy=(axial_location_of_cut_off, desired_amplitude_for_cut_off),
        xytext=(0.5,0.5 ),
        arrowprops=dict(
            arrowstyle="->",
            connectionstyle="angle3,angleA=90,angleB=0"))
ax.plot(axial_location_of_cut_off,desired_amplitude_for_cut_off, marker = 'o')
print(axial_location_of_cut_off)
plt.show()

    

