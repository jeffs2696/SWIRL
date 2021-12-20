 
! Returns M_theta and the corresponding sound speed as defined in
! SourceTermSymbolicSolver.ipynb

    SUBROUTINE CalcSoundSpeed(& 
    r                 , &
    k, kappa          , &
    SoundSpeedExpected, &
    thetaMachData     , &
    axialMachData)
    
    REAL(KIND=rDef)   , INTENT(IN) :: &
    kappa
    
    REAL(KIND=rDef)   , DIMENSION(:), INTENT(INOUT) :: &
    SoundSpeedExpected, thetaMachData, axialMachData
    
    REAL(KIND=rDef)   , DIMENSION(:), INTENT(IN) :: &
    r
    
    COMPLEX(KIND=rDef), DIMENSION(:), INTENT(IN) :: &
    k
     
    ! Local variables 
    INTEGER :: &
    numberOfGridPoints, i!, j
    
    REAL(KIND = rDef) :: one,two,three
    
    REAL(KIND=rDef), DIMENSION(SIZE(k)) :: kR
    
    kR = REAL(k,KIND = rDef)
        
    one   = (1.0_rDef)    
    two   = (2.0_rDef)    
    three = (3.0_rDef)

    
    numberOfGridPoints = SIZE(SoundSpeedExpected)

        DO i = 1,numberOfGridPoints

        SoundSpeedExpected(i) = 0.001d0*tanh(100.0d0*r(i)- 100.0d0) + 0.001d0*tanh(100.0d0*r(i)- 75.0d0) + &
      0.001d0*tanh(100.0d0*r(i)- 50.0d0) + 0.998d0
        thetaMachData(i)      = 1.58113883008419d0*sqrt(r(i)*(-0.200400801603206d0*tanh(100.0d0*r(i)- 100.0d0 &
      )**2.0_rDef - 0.200400801603206d0*tanh(100.0d0*r(i)- 75.0d0)**2.0_rDef - &
      0.200400801603206d0*tanh(100.0d0*r(i)- 50.0d0)**2.0_rDef + &
      0.601202404809619d0)/(0.00100200400801603d0*tanh(100.0d0*r(i)- &
      100.0d0) + 0.00100200400801603d0*tanh(100.0d0*r(i)- 75.0d0) + &
      0.00100200400801603d0*tanh(100.0d0*r(i)- 50.0d0) + 1))
        axialMachData(i)      = 0.01d0*tanh(125.0d0*r(i)- 125.0d0) + 0.01d0*tanh(125.0d0*r(i)- &
      98.333333333333343d0) + 0.01d0*tanh(125.0d0*r(i)- &
      71.666666666666671d0) + 0.01d0*tanh(125.0d0*r(i)- 45.0d0) + 0.17d0


        END DO

    END SUBROUTINE CalcSoundSpeed
