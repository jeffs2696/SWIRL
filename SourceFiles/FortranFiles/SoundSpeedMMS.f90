 
    ! Returns M_theta and the corresponding sound speed as defined in
    ! SourceTermSymbolicSolver.ipynb
    
    SUBROUTINE CalcSoundSpeed(& 
    r                 , &
    SoundSpeedExpected, &
    thetaMachData     , &
    axialMachData)
           
    
    REAL(KIND=rDef)   , DIMENSION(:), INTENT(INOUT) :: &
    SoundSpeedExpected, thetaMachData, axialMachData
    
    REAL(KIND=rDef)   , DIMENSION(:), INTENT(IN) :: &
    r
    
    
    ! Local variables 
    INTEGER :: &
    numberOfGridPoints, i!, j
    
    numberOfGridPoints = SIZE(SoundSpeedExpected)
    
    DO i = 1,numberOfGridPoints
    
            SoundSpeedExpected(i) = 0.0625d0*tanh(0.033333333333333333d0*r(i) - 0.033333333333333333d0) + &
      0.0625d0*tanh(0.033333333333333333d0*r(i) - 0.018333333333333333d0) &
      + 0.0625d0*tanh(0.033333333333333333d0*r(i) - &
      0.0033333333333333335d0) + 0.997188132603746d0
        thetaMachData(i)      = 1.58113883008419d0*sqrt(r(i)*1d0/(0.062676237268094d0*tanh( &
      0.033333333333333333d0*r(i) - 0.033333333333333333d0) + &
      0.062676237268094d0*tanh(0.033333333333333333d0*r(i) - &
      0.018333333333333333d0) + 0.062676237268094d0*tanh( &
      0.033333333333333333d0*r(i) - 0.0033333333333333335d0) + 1)*( &
      -0.00417841581787294d0*tanh(0.033333333333333333d0*r(i) - &
      0.033333333333333333d0)**2 - 0.00417841581787294d0*tanh( &
      0.033333333333333333d0*r(i) - 0.018333333333333333d0)**2 - &
      0.00417841581787294d0*tanh(0.033333333333333333d0*r(i) - &
      0.0033333333333333335d0)**2 + 0.0125352474536188d0))
        axialMachData(i)      = 0.5

    
    END DO
    
    END SUBROUTINE CalcSoundSpeed
    