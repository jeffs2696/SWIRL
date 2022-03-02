import flow_class as fc
import sympy as sp

from flow_class import FlowClass 
from sympy import Symbol



r = sp.Symbol('r')
M_x = sp.Symbol('M_x') 
function = sp.tanh('r')

flow_1 = fc.FlowClass(
        radius = r,
        ratio_of_specific_heats = 1.4,
        axial_mach = M_x,
        sound_speed = function)

print(flow_1.get_tangential_mach())




