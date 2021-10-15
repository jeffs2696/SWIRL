    FORMAT_MEAN_FLOW           = "(F15.12,F15.12,F15.12,F15.12,F15.12)" 
    FORMAT_MEAN_FLOW_HEADER    = "(A15,A15,A15,A15,A15)" 
    FORMAT_PERTURB_VARS        = "(F16.12,F16.12,F16.12,F16.12,F16.12)"
    FORMAT_PERTURB_HEADER      = "(A12,A12,A12,A12,A12)"
    FORMAT_SOURCE_TERMS        = "( I4, F16.12, F16.12,F16.12)" 
    FORMAT_SOURCE_TERMS_HEADER = "( A5, A17, A17,A17)" 
    FORMAT_L2                  = "(I4,F16.12)" 
    FORMAT_L2_HEADER           = "(A5,A13)" 
    FORMAT_ERROR               = "(F16.12,F16.12)" 
    FORMAT_ERROR_HEADER        = "(A17,A17)" 
    FORMAT_ROC                 = "(F16.12,F16.12)" 
    FORMAT_ROC_HEADER          = "(A17,A15)" 

    ! inputs needed for SwirlClassType
    azimuthalModeNumber       = 1
    hubToTipRatio             = r_min/r_max
    frequency                 =  CMPLX(10.0, 0, rDef)
    hubAdmittance             =  CMPLX(0,0,rDef)!CMPLX(0.40, 0 ,rDef)
    ductAdmittance            =  CMPLX(0,0,rDef)!CMPLX(0.70,0,rDef)
    finiteDiffFlag            =  1
    secondOrderSmoother       =  0.0_rDef
    fourthOrderSmoother       =  0.0_rDef

    ! constants needed for calculations
    gam = 1.4_rDef               ! ratio of specific heats
    gm1 = gam-1.0_rDef

    ci  = CMPLX(0.0, 1.0, rDef)  !imaginary number

    ! constants for MMS module
    boundingConstant = 1.00_rDef
    eigenIndex = 1

