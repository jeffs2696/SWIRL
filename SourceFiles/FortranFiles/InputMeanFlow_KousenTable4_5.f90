ALLOCATE( &
axialMachData(numberOfGridPoints) , &
thetaMachData(numberOfGridPoints) , &
totalMachData(numberOfGridPoints)) 

r_max_shankar    = r_min/(1.0_rDef - r_min)
plsmns           = r_max_shankar + 0.50_rDef 
    
DO i = 2, numberOfGridPoints-1

r(i)             = (r_min+REAL(i-1, rDef)*dr)/r_max
r_shankar        = r(i)/(1.0_rDef - r_min)
    IF (r_min.gt.0.0_rDef) THEN
        pwl = 1.0_rDef - 2.0_rDef*ABS(r_max_shankar + 0.50 - r_max_shankar)
        axialVelocity(i) = 
! T.4.5 profile (sheared flow


thetaMachData(i) = 0.0_rDef
totalMachData(i) = (axialMachData(i)**2.0_rDef + &
thetaMachData(i)**2.0_rDef)**0.5
WRITE(0,*) r(i) , axialMachData(i)
ENDDO

