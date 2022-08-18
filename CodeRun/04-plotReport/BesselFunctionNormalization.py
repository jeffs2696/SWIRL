#!/usr/bin/env python
# coding: utf-8

# In[1]:

import sys
import math
from plotReportLib import myfunctions as fcn
import numpy as np
import scipy as scip 
import sympy
from sympy import Symbol,integrate 
from scipy import integrate
from scipy import special
import cmath
import matplotlib as mpl
import matplotlib.pyplot as plt
# mpl.use('pgf')
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

    A = np.sqrt(1/int_psi_square)
    return A

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
def bracket(ax, pos=[0,0], scalex=1, scaley=1, text="",textkw = {}, linekw = {}):
    x = np.array([0, 0.05, 0.45,0.5])
    y = np.array([0,-0.01,-0.01,-0.02])
    x = np.concatenate((x,x+0.5)) 
    y = np.concatenate((y,y[::-1]))
    ax.plot(x*scalex+pos[0], y*scaley+pos[1], clip_on=False, 
            transform=ax.get_xaxis_transform(), **linekw)
    ax.text(pos[0]+0.5*scalex, (y.min()-0.01)*scaley+pos[1], text, 
            transform=ax.get_xaxis_transform(),
            ha="center", va="top", **textkw)

def trapz(f,a,b,N=50):
    '''Approximate the integral of f(x) from a to b by the trapezoid rule.

    The trapezoid rule approximates the integral \int_a^b f(x) dx by the sum:
    (dx/2) \sum_{k=1}^N (f(x_k) + f(x_{k-1}))
    where x_k = a + k*dx and dx = (b - a)/N.  
    Parameters
    ----------
    f : function
    Vectorized function of a single variable
    a , b : numbers
    Interval of integration [a,b]
    N : integer
    Number of subintervals of [a,b] 
    Returns
    -------
    float
    Approximation of the integral of f(x) from a to b using the
    trapezoid rule with N subintervals of equal length.  
    Examples
    -------->>> 
    trapz(np.sin,0,np.pi/2,1000)
    0.9999997943832332
    '''
    x = np.linspace(a,b,N+1) # N+1 points make N subintervals
    y = f(x)
    y_right = y[1:] # right endpoints
    y_left = y[:-1] # left endpoints
    dx = (b - a)/N
    T = (dx/2) * np.sum(y_right + y_left)
    return T



# Plot Settings

plt.style.use('seaborn-whitegrid')
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

# Input Variables for test case in CodeRun directory
M_x     = 0.3
k       = 10

# Compute zeros of integer-order Bessel function derivatives Jn'.
m_order = 2 #azimuthal mode
rad_order = 2 # subtracted by one later to keep indicies inline

k_x_convection = k / M_x
k_x_cutoff = (k*M_x)/(M_x**2-1)


num_of_zeros = 10 # each zero corresponds to radial modes
# the first zero is radial mode 0
# the second zero is radial mode 1...

x_min   = 0
x_max   = 1 
x_steps = 200
x       = np.linspace(x_min,x_max,x_steps)

# Bessel Function Calculation
Jv_p_zero = scip.special.jnp_zeros(n  = m_order, nt = num_of_zeros)
Jv        = scip.special.jv( m_order, x)
Jv_at_Jv_p_zero        = scip.special.jv( m_order, Jv_p_zero)
# print(Jv_p_zero)
# print(Jv_at_Jv_p_zero)



fig = plt.figure(
    constrained_layout=False,
    figsize=set_size(width)
)

plt.plot(x,Jv)
plt.plot(Jv_p_zero[0],0,marker='.')
plt.plot(Jv_p_zero[1],0,marker='.')
plt.ylabel('$$ J_{modeorder:.0f}(k_r r)$$'.format(modeorder = m_order))
plt.xlabel('$$ k_r r$$')
plt.title('Bessel function of the first kind of order ' + str(m_order))

k_x = []
k_x_down = []
k_x_real_down = []
k_x_imag_down = []
k_x_up = []
k_x_real_up = []
k_x_imag_up = []
pm = np.array([1,-1])

for i,j in enumerate(Jv_p_zero):

    k_x.append(((-M_x*k + pm* cmath.sqrt(k**2 - (1-M_x**2)*Jv_p_zero[i]**2))/(1-M_x**2)))
    k_x_down.append(((-M_x*k + cmath.sqrt(k**2 - (1-M_x**2)*Jv_p_zero[i]**2))/(1-M_x**2)))
    k_x_up.append(((-M_x*k - cmath.sqrt(k**2 - (1-M_x**2)*Jv_p_zero[i]**2))/(1-M_x**2)))


    # print('Upstream',i,k_x_up[i])
    # print('Downstream',i,k_x_down[i])

    k_x_real_up.append(k_x_up[i].real)
    k_x_imag_up.append(k_x_up[i].imag)
    k_x_real_down.append(k_x_down[i].real)
    k_x_imag_down.append(k_x_down[i].imag)
# print(k_x_down)
k_x = axial_wavenumber_quadratic(k,M_x,Jv_p_zero)

fig,ax = plt.subplots(
        constrained_layout=False,
        figsize=set_size(width)
)
for i,j in enumerate(Jv_p_zero):
    # print(((k_x[0][i]))) 
    k_x_real_plus = k_x[0][i].real
    k_x_real_minus = k_x[1][i].real
    k_x_imag_plus = k_x[1][i].imag
    k_x_imag_minus = k_x[0][i].imag
    plt.scatter(k_x_real_plus,k_x_imag_plus, marker ='.' , c='red')
    plt.scatter(k_x_real_minus,k_x_imag_minus, marker ='.' , c='black')
    # print(k_x_imag_plus)
    if k_x_imag_plus > 0 or k_x_imag_plus < 0:
        ax.annotate(r'$k^+_{{{M},{N}}}$'.format(M = m_order,N = i+1),
                xy=(k_x_real_plus-k_x_real_plus/4,k_x_imag_plus+k_x_imag_plus/7),
                xycoords='data',
                fontsize='8')

plt.axvline(x = k_x_cutoff,color = 'black', label = 'cut-off line',lw=1)
# plt.show()
# sys.exit()
# plt.scatter(k_x[0][:].real,k_x[0][:].imag,label='Upstream',marker='.')
# plt.scatter(k_x[:][1].real,k_x[:][1].imag,label='Downstream',marker='.') 
# plt.scatter(k_x_real_down,k_x_imag_down,label='Downstream',marker='.')
# plt.scatter(k_x_convection,0,marker='.',label='$$k_{x,cv}$$')
plt.legend()
plt.xlabel(r'\textit{Real}$(k_x)$')
plt.ylabel(r'\textit{Imaginary}$(k_x)$')
plt.show()
# print(Jv)
# print(x)
# x = np.linspace(0.25,1,100)
# print(k_x_up)

print(k_x[0][rad_order-1])
axial_mode =np.exp(-1j*k_x[0][rad_order-1]*x) 

radial_mode = (Jv_at_Jv_p_zero[rad_order]*x)*axial_mode 
# print(Jv_at_Jv_p_zero)
fig = plt.figure()
plt.plot(x,radial_mode,label='Unnormalized')
plt.title(r'$$p_{{{M},{N}}} = J_{{{M}}}(\alpha_{{{M},{N}}}r) e^{{i k_{{x{M},{N}}}r}}$$'.format(M = m_order,N = rad_order))
plt.xlabel(r'Radius, $\bar{r}$')

A = normalize_psi((radial_mode),x)
radial_mode_normalized = A*radial_mode


plt.plot(x,radial_mode_normalized,label='Normalized - Numerical')
print('Numerical Integral',np.trapz(np.conj(radial_mode_normalized)*radial_mode_normalized*x,x))



N_mn = np.sqrt(2)/(Jv_at_Jv_p_zero[rad_order]*np.sqrt(1 - m_order**2/Jv_p_zero[rad_order]**2))
# N_mn = np.sqrt(2)
U_mn = N_mn*(Jv_at_Jv_p_zero[rad_order]*x)*axial_mode
plt.plot(x,U_mn,label='Normalized - Analytic')

# print(np.trapz(np.conj(N_mn)*N_mn*(Jv_at_Jv_p_zero[rad_order-1]*x)**2*x,x))
print('Analytic Integral' ,np.trapz(N_mn**2*(Jv_at_Jv_p_zero[rad_order]*x)**2*x,x))

plt.legend()
plt.show()
#x_sym = sympy.Symbol('x')

#y = abs(np.sin(2*np.pi*x))**2
## y_norm_numerical = np.trapz(y_norm_analytic(x),x)
## y_norm2 = sympy.integrate(y_norm,sympy.Symbol('x'))
## lam_y_norm2 = sympy.lambdify(x_sym, y_norm2, modules =['numpy'])
## y_vals = lam_y_norm2(x) 
## print(y_norm_numerical)
#fig = plt.figure()
#plt.plot(x,y)
## plt.plot(x,y_norm_analytic,label='Normalized')

## plt.plot(x,y_vals)
## sympy.plotting.plot(sympy.Symbol('x'),y_norm2)#,marker = 'o' )
#plt.show()
'''
f = lambda x : radial_mode #abs(np.sqrt(2)*np.sin(2*np.pi*x))**2 
a = x_min; b = x_max; N = 10

# x and y values for the trapezoid rule
x = np.linspace(a,b,N+1)
y = f(x)

# X and Y values for plotting y=f(x)
X = np.linspace(a,b,100)
Y = f(X)
plt.plot(X,Y,label ='Expected Function')

for i in range(N):
    xs = [x[i],x[i],x[i+1],x[i+1]]
    ys = [0,f(x[i]),f(x[i+1]),0]
    print(xs,ys)
    plt.fill(xs,ys,'b',edgecolor='b',alpha=0.2)


plt.title('Trapezoid Rule, N = {}'.format(N))

# y_norm_analytic = abs(np.sqrt(2)*np.sin(2*np.pi*x))**2
 # plt.plot(x,y_norm_analytic, label = ' Approximated Normalized Function')
# plt.plot(x,abs(np.sin(2*np.pi*x)),label = 'Unnormalized Function')
plt.legend()
plt.show()
T = trapz(f,a,b,N)
print(T)
print('error:',T-1)
'''
