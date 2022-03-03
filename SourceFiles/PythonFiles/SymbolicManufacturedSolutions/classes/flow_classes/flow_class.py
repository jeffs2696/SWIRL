#!/usr/bin/python3
import sympy as sp
import logging
from sympy import Symbol

logger = logging.getLogger(__name__)
logger.setLevel(logging.INFO)

formatter = logging.Formatter('%(levelname)s: %(name)s: %(message)s')

file_handler = logging.FileHandler('flow_class.log')
file_handler.setFormatter(formatter)

logger.addHandler(file_handler)

class FlowClass:
    """
    A class used generate the mean flow expression for SWIRL

    ...
    
    Attributes
    ----------
    ratio_of_specific_heats : sympy.Symbol
        a symbol used for the ratio of specific heats

    Methods
    -------
    get_tangential_mach()
        defines the tangential mach number from the speed of sound which is 
        from equation (2.6) 



    """
    #ratio_of_specific_heats = sp.Symbol('kappa')

    def __init__(self,radius,ratio_of_specific_heats,axial_mach,sound_speed):
        """ Defining the flow expressions and allows for user to input a function

        up front by using kwargs
        Parameters
        ----------
        radius : sympy.Symbol
        
        ratio_of_specific_heats :

        axial_mach : sympy.Symbol

        sound_speed:
        

        """ 
        self.radius = radius
        self.ratio_of_specific_heats = ratio_of_specific_heats
        self.axial_mach = axial_mach
        self.sound_speed = sound_speed
        self.tangential_mach = sp.Symbol('M_t') 
    
    def get_tangential_mach(self):
        """A class method that defines the tangential mach number from the 
        speed of sound
        """
        dA_dr = sp.diff(self.sound_speed,self.radius)
        dA_squared_dr = sp.diff(self.sound_speed**2.0,self.radius)

        self.tangential_mach = (
                self.radius/(
                    (self.ratio_of_specific_heats - 1.0)*self.sound_speed**2.0
                    )*dA_squared_dr)**0.5

                
        logger.info(str(self.tangential_mach))

        return self.tangential_mach

