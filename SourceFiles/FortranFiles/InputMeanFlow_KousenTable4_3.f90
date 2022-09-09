ALLOCATE(&
    axialMachData(numberOfGridPoints), &
    thetaMachData(numberOfGridPoints), &
    totalMachData(numberOfGridPoints) )


DO i = 1, numberOfGridPoints

r(i)             = (r_min+REAL(i-1, rDef)*dr)/r_max

! Plug flow for T4.1
axialMachData(i) = 0.5_rDef

thetaMachData(i) = 0.0_rDef

totalMachData(i) = (axialMachData(i)**2.0_rDef + &
thetaMachData(i)**2.0_rDef)**0.5
END DO
