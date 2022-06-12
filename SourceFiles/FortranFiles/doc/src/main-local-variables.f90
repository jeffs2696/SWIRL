    ! inputs needed for SwirlClassType
    azimuthalModeNumber       =  1
    hubToTipRatio             =  r_min/r_max
    numericalIntegrationFlag  =  1
    finiteDiffFlag            =  1
    secondOrderSmoother       =  0.0_rDef
    fourthOrderSmoother       =  0.0_rDef

    IF (numericalIntegrationFlag.eq.1) THEN
        ExpectedRateOfConvergenceSoundSpeed = 2.0_rDef
    ELSEIF (numericalIntegrationFlag.eq.2) THEN 
        ExpectedRateOfConvergenceSoundSpeed = 4.0_rDef
    ENDIF

    IF (finiteDiffFlag.eq.1) THEN
        ExpectedRateOfConvergenceSourceTerm = 2.0_rDef
    ELSEIF (finiteDiffFlag.eq.2) THEN 
        ExpectedRateOfConvergenceSourceTerm = 4.0_rDef
    ENDIF

    ! constants needed for calculations
    ! gam = 1.4_rDef               ! ratio of specific heats
    gm1 = gam-1.0_rDef

    ci  = CMPLX(0.0, 1.0, rDef)  !imaginary number

    ! constants for MMS module
    boundingConstant = 1.00_rDef
    eigenIndex = 1

    facCount = 0 ! initializer for fac count

