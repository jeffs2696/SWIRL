    FORMAT_MEAN_FLOW           = "(F15.12,F15.12,F15.12,F15.12,F15.12)" 
    FORMAT_MEAN_FLOW_HEADER    = "(A15,A15,A15,A15,A15)" 
    FORMAT_PERTURB_VARS        = "(F16.12,F16.12,F16.12,F16.12,F16.12)"
    FORMAT_PERTURB_HEADER      = "(A12,A12,A12,A12,A12)"
    FORMAT_SOURCE_TERMS        = "( I4, F16.12, F16.12,F16.12)" 
    FORMAT_SOURCE_TERMS_HEADER = "( A5, A17, A17,A17)" 
    FORMAT_L2                  = "(I10,F20.16)" 
    FORMAT_L2_HEADER           = "(A10,A20)" 
    FORMAT_ERROR               = "(F20.16,F20.16)" 
    FORMAT_ERROR_HEADER        = "(A20,A20)" 
    FORMAT_ROC                 = "(I10,F20.16)" 
    FORMAT_ROC_HEADER          = ("(A10,A20)")

    ! inputs needed for SwirlClassType
    azimuthalModeNumber       =  1
    hubToTipRatio             =  r_min/r_max
    finiteDiffFlag            =  1
    secondOrderSmoother       =  0.0_rDef
    fourthOrderSmoother       =  0.0_rDef

    ! constants needed for calculations
    ! gam = 1.4_rDef               ! ratio of specific heats
    gm1 = gam-1.0_rDef

    ci  = CMPLX(0.0, 1.0, rDef)  !imaginary number

    ! constants for MMS module
    boundingConstant = 1.00_rDef
    eigenIndex = 1

