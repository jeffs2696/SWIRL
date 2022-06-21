import matplotlib.pyplot as mpl
import sympy as sp
import numpy as np 
from packages.manufactured_solution_generators import generators as msg
import sympy as sp
from packages.symbolic_helpers import sympy_helpers as sp_help

r, r_max, r_min = sp.symbols('x r_max r_min')

r_min = 0.1
r_max = 1

f =msg.TanhMethod(3,3,r_min,r_max)
print(f)

fig = mpl.figure(figsize=(10,6))
ax = mpl.axes()
mpl.plot(r,f)
