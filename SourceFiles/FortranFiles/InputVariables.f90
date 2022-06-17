

    r_min  = 0.100_rDef
    r_max                     = 1.00_rDef
    hubToTipRatio             =  r_min/r_max

    azimuthalModeNumber       =  2
    numericalIntegrationFlag  =  1
!    DO FDfac = 1,2

        secondOrderSmoother       =  0.0_rDef
        fourthOrderSmoother       =  0.0_rDef
        ductAdmittance            = CMPLX(0.1,0.0,rDef)
        hubAdmittance             = CMPLX(0.1,0.0,rDef)
        frequency                 = CMPLX(-1.0,0,rDef)

        gam = 1.4_rDef


