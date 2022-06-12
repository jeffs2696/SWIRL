 
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
    
            SoundSpeedExpected(i) = 0.125d0*tanh(0.033333333333333333d0*r(i) - 0.033333333333333333d0) + 1
        thetaMachData(i)      = 1.58113883008419d0*sqrt(r(i)*(0.00833333333333333d0 - 0.00833333333333333d0 &
      *tanh(0.033333333333333333d0*r(i) - 0.033333333333333333d0)**2)*1d0/ &
      (0.125d0*tanh(0.033333333333333333d0*r(i) - 0.033333333333333333d0) &
      + 1))
        axialMachData(i)      = 0.025d0*tanh(55.555555555555557d0*r(i) - 55.555555555555557d0) + 0.2d0

    
    END DO
    
    END SUBROUTINE CalcSoundSpeed
    