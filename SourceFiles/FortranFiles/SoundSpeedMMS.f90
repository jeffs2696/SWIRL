 
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
        axialMachData(i)      = 0.0125d0*tanh(55.555555555555557d0*r(i) - 55.555555555555557d0) + 0.0125d0* &
      tanh(55.555555555555557d0*r(i) - 43.055555555555557d0) + 0.0125d0* &
      tanh(55.555555555555557d0*r(i) - 30.555555555555557d0) + 0.0125d0* &
      tanh(55.555555555555557d0*r(i) - 18.055555555555554d0) + 0.0125d0* &
      tanh(55.555555555555557d0*r(i) - 5.5555555555555562d0) + &
      0.250000000000347d0

    
    END DO
    
    END SUBROUTINE CalcSoundSpeed
    