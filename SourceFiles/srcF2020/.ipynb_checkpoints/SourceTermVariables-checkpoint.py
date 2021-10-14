# Defining symbolic variables needed for this code.
# all units are dimensionless! 

# radius, maximum radius, and ratio of specific heats
r, r_max, kappa = symbols('r r_max kappa')

# arbitrary constants
k = symbols('k', cls=IndexedBase)
one, two, three = symbols('one two three')
C = symbols('C')
x = symbols('x')

# reduced frequency
ak = symbols('ak')

# imaginary #, Speed of sound, azimuthal mode number, axial wavenumber 
i, A, m, gamma   = symbols('i A m gamma')
v_r, v_t, v_x, p = symbols('v_r v_t v_x p ')
M_t, M_x         = symbols('M_t M_x')
dp_dr, dv_r_dr   = symbols('dp_dr dv_r_dr')
dM_x_dr, dM_t_dr = symbols('dM_x_dr dM_t_dr')

#locations of inflection Points for tanh?
r2 , r3  = symbols('r2 r3')
j        = symbols('j')