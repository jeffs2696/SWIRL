#!/usr/bin/python3
import matplotlib.pyplot as plt

def plot_axial_wavenumbers(
        real_part,
        imag_part,
        azimuthal_mode_number, 
        radial_mode_number,
        wavenumber,
        axial_mach_number,
        ax=None,
        scatter_kwargs={}
        ):

    if ax is None:
        ax = plt.gca() 

    plt.scatter(
            real_part,
            imag_part,
            **scatter_kwargs
            )

    caption = "Complex axial wavenumbers for \
            $m = {M}$, \
            $k = {wavenumber}$, \
            $M_x = {axial_mach_number}$, \
            $\eta = 0$".format(
            M = azimuthal_mode_number,
            wavenumber = wavenumber , 
            axial_mach_number = axial_mach_number )

    plt.xlabel(
            r'\begin{center}\textit{Real}$(k_x)$\\*\textit{\small{' +
            caption +
            r'}}\end{center}')

    plt.ylabel(r'\textit{Imaginary}$(k_x)$')

    if real_part >= 0 and imag_part <= 0:
        # if the wavenumber has a positive real and negative imag
        string_for_annotation = '$k^+_{{{M},{N}}}$'.format(
                M = azimuthal_mode_number ,
                N = radial_mode_number)

    elif real_part <= 0 and imag_part >= 0:
        # if the wavenumber has a negative real and positive imag
        string_for_annotation = '$k^-_{{{M},{N}}}$'.format(
                M = azimuthal_mode_number ,
                N = radial_mode_number)
    else:
        # added this to get the right label for neg imag part
        string_for_annotation = '$k^+_{{{M},{N}}}$'.format(
                M = azimuthal_mode_number ,
                N = radial_mode_number)

    if imag_part > 0 or imag_part < 0:
        # if imaginary part has any value (i.e. not sitting on real axis)
        ax.annotate(
                string_for_annotation,
                xy=(
                    real_part ,
                    imag_part 
                    ),
                xycoords='data', 
                textcoords='offset points',
                xytext = (10,20),
                horizontalalignment='center',
                verticalalignment ='top',
                fontsize='8') 
    else:
        # if the imaginary part is zero then 
        ax.annotate(
                string_for_annotation,
                xy=(
                    real_part, 
                    imag_part
                    ),
                xycoords='data',
                textcoords='offset points',
                xytext = (0,20),
                horizontalalignment='center',
                verticalalignment ='top',
                fontsize='8')

    # if axial_mach_number > 0:        
    #         k_x_cutoff = axial_mach_number*wavenumber/(axial_mach_number**2-1)
    #         plt.axvline(
    #                 x = k_x_cutoff,
    #                 color = 'black',
    #                 label = 'cut-off line',
    #                 lw=0.5,
    #                 ls='dotted') 

    return ax 
