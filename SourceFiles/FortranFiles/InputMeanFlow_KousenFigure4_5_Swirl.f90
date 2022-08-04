ALLOCATE(&
    axialMachData(numberOfGridPoints), &
    thetaMachData(numberOfGridPoints), &
    totalMachData(numberOfGridPoints) )


DO i = 1, numberOfGridPoints

r(i)             = (r_min+REAL(i-1, rDef)*dr)/r_max

! Plug flow for T4.1
axialMachData(i) = 0.3_rDef

thetaMachData(i) = 0.2/r(i) + axialMachData(i) 

totalMachData(i) = (axialMachData(i)**2.0_rDef + &
thetaMachData(i)**2.0_rDef)**0.5
END DO
