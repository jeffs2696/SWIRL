#!/usr/bin/python3
import sympy as sp
from sympy import Symbol

class FlowClass:

    # class variables
    # - defining the class symbolic variable
    M_x = sp.Symbol('M_x') 
  
    
    def __init__(self):
        self.AxialMach = sp.Symbol('M_x') 
        self.SoundSpeed =sp.Symbol('A')
        

    #def get_tangential_mach(self):
    #    self.TangentialMach = sp.Symbol('1')
#
function = sp.cos('r')
flow_1 = FlowClass()
print(flow_1.AxialMach)
