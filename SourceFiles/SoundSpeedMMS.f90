 
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

        SoundSpeedExpected(i) = 0.1d0*tanh(0.10000000000000001d0*r(i)- 0.10000000000000001d0) + 0.1d0*tanh &
      (0.10000000000000001d0*r(i)- 0.075000000000000011d0) + 0.1d0*tanh( &
      0.10000000000000001d0*r(i)- 0.050000000000000003d0) + &
      0.99250468320737d0
        thetaMachData(i)      = 1.58113883008419d0*sqrt(r(i)*(-0.0201510384166331d0*tanh( &
      0.10000000000000001d0*r(i)- 0.10000000000000001d0)**2.0_rDef - &
      0.0201510384166331d0*tanh(0.10000000000000001d0*r(i)- &
      0.075000000000000011d0)**2.0_rDef - 0.0201510384166331d0*tanh( &
      0.10000000000000001d0*r(i)- 0.050000000000000003d0)**2.0_rDef + &
      0.0604531152498994d0)/(0.100755192083166d0*tanh( &
      0.10000000000000001d0*r(i)- 0.10000000000000001d0) + &
      0.100755192083166d0*tanh(0.10000000000000001d0*r(i)- &
      0.075000000000000011d0) + 0.100755192083166d0*tanh( &
      0.10000000000000001d0*r(i)- 0.050000000000000003d0) + 1))
        axialMachData(i)      = 0.00833333333333333d0*tanh(125.0d0*r(i)- 125.0d0) + 0.00833333333333333d0* &
      tanh(125.0d0*r(i)- 100.0d0) + 0.00833333333333333d0*tanh(125.0d0*r(i)&
      - 75.0d0) + 0.00833333333333333d0*tanh(125.0d0*r(i)- 50.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r(i)- 25.0d0) + &
      0.166666666666667d0


        END DO

    END SUBROUTINE CalcSoundSpeed
