#!/usr/bin/env python
import helpers.analytic_functions as afcn

class DuctModeClass:
    def __init__(
            self,
            num_of_zeros,
            wavenumber,
            radial_mode_number,
            azimuthal_mode_number,
            axial_mach_number,
            r_min = 0,
            r_max = 1):

        self.num_of_zeros = num_of_zeros
        self.wavenumber = wavenumber
        self.radial_mode_number = radial_mode_number
        self.azimuthal_mode_number = azimuthal_mode_number
        self.axial_mach_number = axial_mach_number
        self.r_min = r_min
        self.r_max = r_max
# Methods Needed
#
#

