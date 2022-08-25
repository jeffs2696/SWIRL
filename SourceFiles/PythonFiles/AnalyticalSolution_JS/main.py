#!/usr/bin/python3
import math
import cmath
import numpy as np
import scipy as scp
from scipy import special
import helpers.helper_functions as fcn
import matplotlib as mpl
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D # needed for 3d plots
from matplotlib import cm
# plot parameters

plt.style.use('bmh')
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

# test case parameters 

# no flow
k = 5
m_order = 3 
rad_order = 3
M_x = 0

angle = np.linspace(0,2*math.pi,100)

x = np.linspace(0,1,100)
t = 0

num_of_zeros = 5 # each zero corresponds to radial modes

Jv_p_zero = scp.special.jnp_zeros(n  = m_order, nt = num_of_zeros)
Jv_p_zero_01 =scp.special.jnp_zeros(n  = 0, nt = 1)
 
# k_x = cmath.sqrt(k**2 - Jv_p_zero[rad_order-1]**2)
k_x_01 = k 
k_x_plus,k_x_minus = fcn.axial_wavenumber_quadratic(k,M_x,Jv_p_zero)

if m_order < 1:
    # append frequency as the first wavenumber when m = 0 
    k_x_plus = np.append(k,k_x_plus)
    k_x_minus = np.append(-k,k_x_minus)

# -----------------------------------------------------------------------------
# plotting data 

fig,ax = plt.subplots(
        constrained_layout=False,
        # figsize=fcn.set_size(345)
)
for i,j in enumerate(Jv_p_zero):

    plt.scatter(k_x_plus[i].real,k_x_plus[i].imag, marker ='.' , c='black')
    plt.scatter(k_x_minus[i].real,k_x_minus[i].imag, marker ='.' , c='black')
    # print(k_x_imag_plus)
    if k_x_plus[i].imag > 0 or k_x_plus[i].imag < 0:
        ax.annotate(r'$k^+_{{{M},{N}}}$'.format(M = m_order,N = i+1),
                xy=(k_x_plus[i].real,k_x_plus[i].imag),
                xycoords='data',
                fontsize='8')
        ax.annotate(r'$k^-_{{{M},{N}}}$'.format(M = m_order,N = i+1),
                xy=(k_x_minus[i].real,k_x_minus[i].imag),
                xycoords='data',
                fontsize='8')
    else:
        ax.annotate(r'$k^+_{{{M},{N}}}$'.format(M = m_order,N = i+1),
                xy=(k_x_plus[i].real,k_x_plus[i].imag),
                xycoords='data',
                fontsize='8')
        ax.annotate(r'$k^-_{{{M},{N}}}$'.format(M = m_order,N = i+1),
                xy=(k_x_minus[i].real,k_x_minus[i].imag),
                xycoords='data',
                fontsize='8')

if M_x > 0:        
    plt.axvline(x = k_x_cutoff,color = 'black', label = 'cut-off line',lw=1) 
    plt.legend()

txt = "Complex axial wavenumbers for $m = 0$, $k = 5$, $M_x = 0$ $\eta = 0$"

plt.xlabel(r'\begin{center}\textit{Real}$(k_x)$\\*\textit{\small{' + txt + r'}}\end{center}')

# resize the figure to match the aspect ratio of the Axes    
fig.set_size_inches(10, 8, forward=True)

# plt.xlabel(r'\textit{Real}$(k_x)$')
plt.ylabel(r'\textit{Imaginary}$(k_x)$')
# print(min(k_x_minus[:].imag))
# plt.ylim(min(k_x_minus[:].imag),max(k_x_plus[:].imag))

plt.savefig(
    fname      ='figures/axial_wavenumber_analytical_test_case.pdf',
    format     ='pdf',
    bbox_inches='tight')

fig = plt.figure()

radial_mode =scp.special.jv(m_order, Jv_p_zero[rad_order-1]*x)
plt.plot(x,radial_mode)


A = fcn.normalize_psi((radial_mode),x)
radial_mode_normalized = A*radial_mode


plt.plot(x,radial_mode_normalized,label='Normalized - Numerical')


# plt.ylabel(r'\textit{Pressure,}$(p)$')




if m_order == 0 and rad_order == 1:
    N_mn = np.sqrt(2)
else: 
    N_mn = np.sqrt(2)/(abs(scp.special.jv(m_order, Jv_p_zero[rad_order-1]))*np.sqrt(1 - m_order**2/Jv_p_zero[rad_order-1]**2))

U_mn = N_mn*(radial_mode)

plt.plot(x,U_mn,label='Normalized - Analytic',linestyle='dotted',lw=2,marker='o')

print('Numerical Integral',np.trapz(np.conj(radial_mode_normalized)*radial_mode_normalized*x,x))
print('Analytic Integral' ,np.trapz(np.conj(U_mn)*U_mn*x,x,dx = 0.0001))

txt = ("Analytical Propagating Mode for " + 
        "$m = {M}$," + 
        "$k_r = {N}$,"+
        "$k = {freq}$,"+
        "$M_x = {speed}$,"+ 
        "$\eta = {liner}$").format(
        M = m_order,
        N = rad_order, 
        freq =k ,
        speed = M_x,
        liner = 0)

plt.xlabel(r'\begin{center}\textit{Radius,}$(r)$\\*\textit{\small{' + txt + r'}}\end{center}')
# plt.figtext(k_x_plus[0],k_x_minus[0],txt, wrap = True, horizontalalignment='center', fontsize=12)
plt.legend()
fig.set_size_inches(10, 8, forward=True)

# plt.show()
# angle = 0
# exponential_coefficient =np.exp(1j*k*t-1j*m_order*angle-1j*k_x_plus[rad_order-1]*x) 


# fig = plt.figure()
# plt.plot(x,exponential_coefficient)

# plt.show()

# fig = plt.figure()
# mode_solution = radial_mode*exponential_coefficient
# plt.plot(x,mode_solution)
# plt.show()

# mode_solution = np.empty([len(x),len(angle)])
# for i in range(len(x)):
#     for j in range(len(angle)):

#         amplitude =scp.special.jv(m_order, Jv_p_zero[rad_order]*x[i])
#         exponential_coefficient =np.exp(1j*k*t-1j*m_order*angle[j]-1j*k_x_plus[rad_order]*x[i]) 


#         mode_solution[i,j] = amplitude*exponential_coefficient
# print(mode_solution)
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')
# ax.plot_surface(angle,x,mode_solution)
# plt.show()

# Create the mesh in polar coordinates and compute corresponding Z.
r = np.linspace(0, 1, 50)
p = np.linspace(0, 2*np.pi, 50)
R, P = np.meshgrid(r, p)

radial_mode =scp.special.jv(m_order, Jv_p_zero[rad_order-1]*R)
Z = radial_mode*np.exp(-1j*m_order*P).real

# Express the mesh in the cartesian system.
X, Y = R*np.cos(P), R*np.sin(P)

# Plot the surface.
ax.plot_surface(X, Y, Z, cmap=plt.cm.YlGnBu_r)
ax.view_init(30,90)

ax = plt.figure().add_subplot(projection='3d')
# X, Y, Z = Axes3d.get_test_data(0.05)

# Plot the 3D surface
# ax.plot_surface(X, Y, Z, rstride=8, cstride=8, alpha=0.3)

ax.plot_surface(X, Y, Z, cmap=plt.cm.YlGnBu_r)
# Plot projections of the contours for each dimension.  By choosing offsets
# that match the appropriate axes limits, the projected contours will sit on
# the 'walls' of the graph.
ax.contourf(X, Y, Z, zdir='z', offset=(max(Z[0])+max(Z[0])/2),cmap =plt.cm.YlGnBu_r)#, cmap=cm.coolwarm)
# ax.contour(X, Y, Z, zdir='x', offset=min(X[0]), cmap=cm.coolwarm)
# ax.contour(X, Y, Z, zdir='y', offset=min(-X[0]), cmap=cm.coolwarm)

ax.set(
        # xlim=(-40, 40), ylim=(-40, 40), zlim=(-100, 100),
        xlabel='X', ylabel='Y', zlabel='P')

# plt.show()

# rotate the axes and update
# for angle in range(0, 360):
#     ax.view_init(30, angle)
#     plt.draw()
#     plt.pause(.01)
# plt.show()
points = 1000
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')


angle_min = 0 
angle_max = 360*2 
dangle = angle_max - angle_min
n = dangle/720
r = 1
# pitch -distance from curve to curve
c = 2*np.pi*m_order/abs(k_x_plus[0].real)
# c =1# 2*np.pi*m_order/abs(k_x_plus[0].real)
# print('n',n)
u = np.linspace(0, r, endpoint=True, num=int(points * n))
v = np.linspace(-np.deg2rad(angle_min), np.deg2rad(angle_max), endpoint=True, num=int(2 * points * n))
u, v = np.meshgrid(u, v)

x = u * np.cos(v)   # r*cos(theta)
y = u * np.sin(v)   # r*cos(theta)
z = c * v + u*np.arctan(m_order*v + k_x_plus[0].real*u) #pitch*theta + r*arctan(phase)
# phase = m*theta + Re(k_x)*x

fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

ax.plot_surface(z, x, y)#, alpha=1, edgecolors='w', zorder=0, color='black')
ax.quiver(-6, 0, 0, 24, 0, 0, length=1, arrow_length_ratio=0.03, color='black', zorder=1)
ax.text(19, -2, -2, "$\mathbf{z}$", fontsize=40, color='black', zorder=1)
# ax._axis3don = False
plt.show()
# z =  np.sin(theta)
# y =  np.cos(theta)
# ax.plot(x, y, z, 'b', lw=2)

# An line through the centre of the helix
# ax.plot((-theta_max*0.2, theta_max * 1.2), (0,0), (0,0), color='k', lw=2)
# sin/cos components of the helix (e.g. electric and magnetic field
# components of a circularly-polarized electromagnetic wave
# ax.plot(x, y, 0, color='r', lw=1, alpha=0.5)
# ax.plot(x, [0]*n, z, color='m', lw=1, alpha=0.5)

# Remove axis planes, ticks and labels
# ax.set_axis_off()
# plt.show()
