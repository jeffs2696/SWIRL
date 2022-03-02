 
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
    
            SoundSpeedExpected(i) = 0.107142857142857d0*tanh(0.0011111111111111111d0*r(i) - &
      0.0011111111111111111d0) + 0.107142857142857d0*tanh( &
      0.0011111111111111111d0*r(i) - 0.00091111111111111124d0) + &
      0.107142857142857d0*tanh(0.0011111111111111111d0*r(i) - &
      0.00071111111111111115d0) + 0.107142857142857d0*tanh( &
      0.0011111111111111111d0*r(i) - 0.00051111111111111105d0) + &
      0.107142857142857d0*tanh(0.0011111111111111111d0*r(i) - &
      0.00031111111111111107d0) + 0.107142857142857d0*tanh( &
      0.0011111111111111111d0*r(i) - 0.00011111111111111112d0) + &
      0.999678571492857d0
        thetaMachData(i)      = 1.58113883008419d0*sqrt(r(i)*1d0/(0.107177306984641d0*tanh( &
      0.0011111111111111111d0*r(i) - 0.0011111111111111111d0) + &
      0.107177306984641d0*tanh(0.0011111111111111111d0*r(i) - &
      0.00091111111111111124d0) + 0.107177306984641d0*tanh( &
      0.0011111111111111111d0*r(i) - 0.00071111111111111115d0) + &
      0.107177306984641d0*tanh(0.0011111111111111111d0*r(i) - &
      0.00051111111111111105d0) + 0.107177306984641d0*tanh( &
      0.0011111111111111111d0*r(i) - 0.00031111111111111107d0) + &
      0.107177306984641d0*tanh(0.0011111111111111111d0*r(i) - &
      0.00011111111111111112d0) + 1)*(-0.000238171793299202d0*tanh( &
      0.0011111111111111111d0*r(i) - 0.0011111111111111111d0)**2 - &
      0.000238171793299202d0*tanh(0.0011111111111111111d0*r(i) - &
      0.00091111111111111124d0)**2 - 0.000238171793299202d0*tanh( &
      0.0011111111111111111d0*r(i) - 0.00071111111111111115d0)**2 - &
      0.000238171793299202d0*tanh(0.0011111111111111111d0*r(i) - &
      0.00051111111111111105d0)**2 - 0.000238171793299202d0*tanh( &
      0.0011111111111111111d0*r(i) - 0.00031111111111111107d0)**2 - &
      0.000238171793299202d0*tanh(0.0011111111111111111d0*r(i) - &
      0.00011111111111111112d0)**2 + 0.00142903075979521d0))
        axialMachData(i)      = 0.0

    
    END DO
    
    END SUBROUTINE CalcSoundSpeed
    