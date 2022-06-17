 
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
    
            SoundSpeedExpected(i) = 0.0416666666666667d0*tanh(0.33333333333333331d0*r(i) - &
      0.33333333333333331d0) + 0.0416666666666667d0*tanh( &
      0.33333333333333331d0*r(i) - 0.25833333333333336d0) + &
      0.0416666666666667d0*tanh(0.33333333333333331d0*r(i) - &
      0.18333333333333335d0) + 0.0416666666666667d0*tanh( &
      0.33333333333333331d0*r(i) - 0.10833333333333332d0) + &
      0.0416666666666667d0*tanh(0.33333333333333331d0*r(i) - &
      0.033333333333333333d0) + 0.969319341472464d0
        thetaMachData(i)      = 1.58113883008419d0*sqrt(r(i)*1d0/(0.0429854898008865d0*tanh( &
      0.33333333333333331d0*r(i) - 0.33333333333333331d0) + &
      0.0429854898008865d0*tanh(0.33333333333333331d0*r(i) - &
      0.25833333333333336d0) + 0.0429854898008865d0*tanh( &
      0.33333333333333331d0*r(i) - 0.18333333333333335d0) + &
      0.0429854898008865d0*tanh(0.33333333333333331d0*r(i) - &
      0.10833333333333332d0) + 0.0429854898008865d0*tanh( &
      0.33333333333333331d0*r(i) - 0.033333333333333333d0) + 1)*( &
      -0.028656993200591d0*tanh(0.33333333333333331d0*r(i) - &
      0.33333333333333331d0)**2 - 0.028656993200591d0*tanh( &
      0.33333333333333331d0*r(i) - 0.25833333333333336d0)**2 - &
      0.028656993200591d0*tanh(0.33333333333333331d0*r(i) - &
      0.18333333333333335d0)**2 - 0.028656993200591d0*tanh( &
      0.33333333333333331d0*r(i) - 0.10833333333333332d0)**2 - &
      0.028656993200591d0*tanh(0.33333333333333331d0*r(i) - &
      0.033333333333333333d0)**2 + 0.143284966002955d0))
        axialMachData(i)      = 0.0166666666666667d0*tanh(11.111111111111111d0*r(i) - 11.111111111111111d0 &
      ) + 0.0166666666666667d0*tanh(11.111111111111111d0*r(i) - &
      1.1111111111111112d0) + 0.183333333402038d0

    
    END DO
    
    END SUBROUTINE CalcSoundSpeed
    