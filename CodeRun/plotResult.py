#!/usr/bin/env python3
import matplotlib
import matplotlib.pyplot as plt
import pandas as pd
import pprint 
import scipy as scip
import numpy as np

debug_flag = True#False

filename = 'radial_mode_data.dat'
parameter_filename = 'radial_mode_parameters.dat'
data = \
        pd.read_csv(filename, delim_whitespace = True)

if debug_flag:
   pprint.pprint(data)

parameters = \
        pd.read_csv(parameter_filename, delim_whitespace = True)
if debug_flag:
    pprint.pprint(parameters)

AMN =float(parameters['weighting_factor_A'])
BMN =float(parameters['weighting_factor_B'])
k_rmn = float(parameters['non_dimensional_roots'])
m_order = float(parameters['azimuthal_mode_number'])

pressure_python = \
        (AMN)*scip.special.jv(m_order,k_rmn*(data['radius'])) + \
        (BMN)*scip.special.yv(m_order,k_rmn*(data['radius']))

fig, axs = plt.subplots()

axs.plot(
        data['radius'],
        data['pressure'],
        label = 'FORTRAN Output (V072)')

axs.plot(data['radius'],pressure_python,'o',label = 'Python Output')

plt.title('Radial Mode for an Annular Duct, m =' + str(m_order))
plt.xlabel('Radius [-]')
plt.ylabel('Pressure [-]')
plt.legend()

# plt.savefig('docs/figures/Figure1.png')

fig, axs = plt.subplots()
error = abs(pressure_python - data['pressure'])

axs.plot(data['radius'],error)
plt.title('Radial Mode Error ')
plt.xlabel('Radius [-]')
plt.ylabel('Pressure [-]')
plt.legend()

# plt.savefig('docs/figures/Figure1error.png')
fig, axs = plt.subplots()
axs.plot(np.diff(data['pressure']))

plt.title('Radial Mode Numerical Derivative for an Annular Duct, m =' + str(m_order))
plt.xlabel('Radial Grid Index [-]')
plt.ylabel('d(Pressure)/dRadius [-]')

# plt.savefig('docs/figures/Figure1deriv.png')
plt.show()



