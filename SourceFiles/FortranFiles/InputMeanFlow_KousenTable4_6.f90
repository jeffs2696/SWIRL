ALLOCATE( &
    axialMachData(numberOfGridPoints) , &
    thetaMachData(numberOfGridPoints) , &
    totalMachData(numberOfGridPoints) , &
    axialVelocity(numberOfGridPoints) , &
    diff_axialVelocity(numberOfGridPoints)  , &
    ) 
WRITE(0,*) 'test'
max_axial_mach_number = 0.3_rDef

r_max_shankar    = r_min/(1.0_rDef - r_min)
plsmns           = r_max_shankar + 0.50_rDef 

DO i = 1, numberOfGridPoints 
r(i)             = (r_min+REAL(i-1, rDef)*dr)/r_max
ENDDO
DO i = 2, numberOfGridPoints-1

r_shankar        = r(i)/(1.0_rDef - r_min)

IF (r_min.gt.0.0_rDef) THEN ! annulus; equation changes. (Given in Shankar Ref 10)

    power_law = 1.0_rDef - 2.0_rDef*ABS(r_max_shankar + 0.50 - r_shankar)
    axialVelocity(i) = max_axial_mach_number*power_law**(1.0_rDef/7.0_rDef)

    IF (r_shankar.le.plsmns) THEN 
        sgn = -1.0_rDef
    ELSE
        sgn = -1.0_rDef
    ENDIF
    diff_axialVelocity(i) = -2.0_rDef*max_axial_mach_number/(7.0_rDef*power_law**(-6.0_rDef/7.0_rDef))*sgn

ELSE ! cylinder 
    axialVelocity(i) = max_axial_mach_number*(1.0_rDef - r_shankar)**(1.0_rDef/7.0_rDef)
    diff_axialVelocity(i) = -max_axial_mach_number/7.*(1. - r_shankar)**(-6.0_rDef/7.0_rDef)
ENDIF
ENDDO
IF (r_min.gt.0.0_rDef) THEN !need to go through Shankar ref for this
    axialVelocity(1) = 0.0_rDef
    diff_axialVelocity(1) = 1.10_rDef*diff_axialVelocity(2)
ELSE !DRH addition; set value at venterline of cylinder (r=0)
    axialVelocity(1) = max_axial_mach_number 
    diff_axialVelocity(1) = 0.0_rDef
ENDIF
axialVelocity(numberOfGridPoints) = 0.0_rDef 
diff_axialVelocity(numberOfGridPoints) = 1.10_rDef*diff_axialVelocity(numberOfGridPoints - 1)
DO i = 1,numberOfGridPoints 
axialMachData(i) = axialVelocity(i)/1.0_rDef
thetaMachData(i) = 0.0_rDef
totalMachData(i) = (axialMachData(i)**2.0_rDef + &
thetaMachData(i)**2.0_rDef)**0.5
! thetaMachData(i) = 0.0_rDef
ENDDO

                
! T.4.5 profile (sheared flow


! WRITE(0,*) r(i) , axialMachData(i)

