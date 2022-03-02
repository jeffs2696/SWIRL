#!/usr/bin/python3
import sympy as sp
from sympy import Symbol

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

    def __init__(self,radius,ratio_of_specific_heats,axial_mach,sound_speed):#**kwargs):
        """ Defining the flow expressions and allows for user to input a function

        up front by using kwargs
        Parameters
        ----------
        radius : sympy.Symbol
        
        axial_mach : sympy.Symbol
        
        kwargs : optional
            If the argument 'sound_speed and tangential_mach are not passed in,
            default functions are set to allow for the use of the 'get_tangential_mach' method
        """ 
        self.radius = radius
        self.ratio_of_specific_heats = ratio_of_specific_heats
        self.axial_mach = axial_mach
        self.sound_speed = sound_speed
        self.tangential_mach = sp.Symbol('M_t') 
#        kwargs.get(
#                'sound_speed',sp.Function('A')(sp.Symbol('r'))
#                )
#        self.tangential_mach = kwargs.get(
#                'tangential_mach',sp.Function('M_t')(sp.Symbol('r'))
#                )
#
   #     if (isinstance(radius,sp.Basic)):
   #     else:
   #         raise TypeError('The radius is not symbolic') 
   # 
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

                
#        if (isinstance(self.tangential_mach,sp.Basic)):
#            print(self.tangential_mach)
#            raise TypeError('self.tangential_mach is not a sympy object')
#
        return self.tangential_mach

    # Integrating is So SLOW 
#    def get_sound_speed(self):
#        self.SoundSpeed = sp.exp(
#                (1-self.RatioOfSpecificHeats)/2 * sp.integrate(
#                    self.TangentialMach**2/self.radius,self.radius
#                    ,sp.Symbol('1')))

