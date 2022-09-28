#!/usr/bin/env python
class DuctModeClass:
    def __init__(
            self,
            num_of_zeros,
            wavenumber,
            azimuthal_mode_number,
            axial_mach_number,
            r_min = 0,
            r_max = 1):

        self.num_of_zeros = num_of_zeros
        self.wavenumber = wavenumber
        self.azimuthal_mode_number = azimuthal_mode_number
        self.axial_mach_number = axial_mach_number
        self.r_min = r_min
        self.r_max = r_max
# Methods Needed
#
#

