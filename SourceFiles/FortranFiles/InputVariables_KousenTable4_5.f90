ALLOCATE( &
axialMachData(numberOfGridPoints) , &
thetaMachData(numberOfGridPoints)) 

DO i = 1, numberOfGridPoints
! T.4.5 profile (sheared flow
axialMachData(i) = 0.3_rDef*(1.0_rDef - 2.0_rDef*ABS(&
(r_min - r(i))/(1.0_rDef/7.0_rDef) + 0.5_rDef))**(1.0_rDef/7.0_rDef)
thetaMachData(i) = 0.0_rDef
ENDDO

