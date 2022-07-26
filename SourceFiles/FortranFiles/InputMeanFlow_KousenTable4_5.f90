ALLOCATE( &
axialMachData(numberOfGridPoints) , &
thetaMachData(numberOfGridPoints) , &
totalMachData(numberOfGridPoints)) 

DO i = 1, numberOfGridPoints

r(i)             = (r_min+REAL(i-1, rDef)*dr)/r_max
! T.4.5 profile (sheared flow
axialMachData(i) = 0.3_rDef*(&
    1.0_rDef - &
    1.0_rDef/(1.0_rDef-(r_min/r_max))**2.0_rDef *&
    ABS(&
    (r_min /r_max + 1.0_rDef- 2.0_rDef*r(i)))**&
    (1.0_rDef/7.0_rDef) )

thetaMachData(i) = 0.0_rDef
totalMachData(i) = (axialMachData(i)**2.0_rDef + &
thetaMachData(i)**2.0_rDef)**0.5
WRITE(0,*) r(i) , axialMachData(i)
ENDDO

