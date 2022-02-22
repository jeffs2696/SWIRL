 
! Returns M_theta and the corresponding sound speed as defined in
! SourceTermSymbolicSolver.ipynb

    SUBROUTINE CalcSoundSpeed(& 
    r                 , &
    kappa          , &
    SoundSpeedExpected, &
    thetaMachData     , &
    axialMachData)
    
    REAL(KIND=rDef)   , INTENT(IN) :: &
    kappa
    
    REAL(KIND=rDef)   , DIMENSION(:), INTENT(INOUT) :: &
    SoundSpeedExpected, thetaMachData, axialMachData
    
    REAL(KIND=rDef)   , DIMENSION(:), INTENT(IN) :: &
    r
    
     
    ! Local variables 
    INTEGER :: &
    numberOfGridPoints, i!, j
    
    REAL(KIND = rDef) :: one,two,three
        
    one   = (1.0_rDef)    
    two   = (2.0_rDef)    
    three = (3.0_rDef)

    
    numberOfGridPoints = SIZE(SoundSpeedExpected)

        DO i = 1,numberOfGridPoints

        SoundSpeedExpected(i) = 0.1875d0*tanh(0.0037499999999999999d0*r(i)- 0.0037499999999999999d0) + &
      0.1875d0*tanh(0.0037499999999999999d0*r(i)- 0.0022500000000000003d0 &
      ) + 0.1875d0*tanh(0.0037499999999999999d0*r(i)- &
      0.00075000000000000002d0) + 0.999156251898431d0
        thetaMachData(i)      = 1.58113883008419d0*sqrt(r(i)*(-0.00140743752273789d0*tanh( &
      0.0037499999999999999d0*r(i)- 0.0037499999999999999d0)**2 - &
      0.00140743752273789d0*tanh(0.0037499999999999999d0*r(i)- &
      0.0022500000000000003d0)**2 - 0.00140743752273789d0*tanh( &
      0.0037499999999999999d0*r(i)- 0.00075000000000000002d0)**2 + &
      0.00422231256821366d0)/(0.187658336365052d0*tanh( &
      0.0037499999999999999d0*r(i)- 0.0037499999999999999d0) + &
      0.187658336365052d0*tanh(0.0037499999999999999d0*r(i)- &
      0.0022500000000000003d0) + 0.187658336365052d0*tanh( &
      0.0037499999999999999d0*r(i)- 0.00075000000000000002d0) + 1))
        axialMachData(i)      = 0.0125d0*tanh(62.5d0*r(i)- 62.5d0) + 0.0125d0*tanh(62.5d0*r(i)- 50.0d0) + &
      0.0125d0*tanh(62.5d0*r(i)- 37.5d0) + 0.0125d0*tanh(62.5d0*r(i)- &
      25.0d0) + 0.0125d0*tanh(62.5d0*r(i)- 12.5d0) + 0.0500000000003472d0


        END DO

    END SUBROUTINE CalcSoundSpeed
