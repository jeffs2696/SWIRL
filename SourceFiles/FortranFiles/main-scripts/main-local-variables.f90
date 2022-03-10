    FORMAT_MEAN_FLOW           = "(F15.12,F15.12,F15.12,F15.12,F15.12)" 
    FORMAT_MEAN_FLOW_HEADER    = "(A15,A15,A15,A15,A15)" 
    FORMAT_PERTURB_VARS        = "(F16.12,F16.12,F16.12,F16.12,F16.12)"
    FORMAT_PERTURB_HEADER      = "(A12,A12,A12,A12,A12)"
    FORMAT_SOURCE_TERMS        = "( I4, F16.12, F16.12,F16.12)" 
    FORMAT_SOURCE_TERMS_HEADER = "( A5, A17, A17,A17)" 
    FORMAT_L2                  = "(I10,F20.12)" 
    FORMAT_L2_HEADER           = "(A10,A20)" 
    FORMAT_ERROR               = "(F20.12,F20.12)" 
    FORMAT_ERROR_HEADER        = "(A20,A20)" 
    FORMAT_ROC                 = "(I10,F20.12)" 
    FORMAT_ROC_HEADER          = ("(A10,A20)")

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


    ALLOCATE(&
        numberOfGridPointsArray(numberOfIterations) ,&
        k(7) , &
        S_L2Array(numberOfIterations)              , &
        SoundSpeedL2Array(numberOfIterations)       ,&
        RateOfConvergence1(numberOfIterations - 1) , &
        RateOfConvergence2(numberOfIterations - 1) )

    facCount = 0 ! initializer for fac count

