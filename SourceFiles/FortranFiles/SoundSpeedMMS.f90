 
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
    
    numberOfGridPoints = SIZE(SoundSpeedExpected)
    
    DO i = 1,numberOfGridPoints
    
            SoundSpeedExpected(i) = 0.375d0*tanh(0.0011111111111111111d0*r(i) - 0.0011111111111111111d0) + 1
        thetaMachData(i)      = 1.58113883008419d0*sqrt(r(i)*(0.000833333333333333d0 - &
      0.000833333333333333d0*tanh(0.0011111111111111111d0*r(i) - &
      0.0011111111111111111d0)**2)*1d0/(0.375d0*tanh( &
      0.0011111111111111111d0*r(i) - 0.0011111111111111111d0) + 1))
        axialMachData(i)      = 0.0

    
    END DO
    
    END SUBROUTINE CalcSoundSpeed
    