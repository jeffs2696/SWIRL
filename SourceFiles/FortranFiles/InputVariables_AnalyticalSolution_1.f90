

r_min                     = 0.50_rDef
r_max                     = 1.00_rDef

azimuthalModeNumber       =  2
numericalIntegrationFlag  =  1
!    DO FDfac = 1,2

secondOrderSmoother       =  0.0_rDef!100.0_rDef
fourthOrderSmoother       =  0.0_rDef!110.0_rDef
ductAdmittance            = CMPLX(0.0,0.0,rDef)
hubAdmittance             = CMPLX(0.0,0.0,rDef)
frequency                 = CMPLX(10.0,0,rDef)

gam                       = 1.4_rDef

