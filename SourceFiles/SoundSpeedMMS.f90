 
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

        SoundSpeedExpected(i) = 0.001d0*tanh(10.0d0*r(i)- 10.0d0) + 0.001d0*tanh(10.0d0*r(i)- 7.5d0) + &
      0.001d0*tanh(10.0d0*r(i)- 5.0d0) + 0.998013476497586d0
        thetaMachData(i)      = 1.58113883008419d0*sqrt(r(i)*(-0.0200398095526603d0*tanh(10.0d0*r(i)- 10.0d0) &
      **2.0_rDef - 0.0200398095526603d0*tanh(10.0d0*r(i)- 7.5d0)**2.0_rDef - &
      0.0200398095526603d0*tanh(10.0d0*r(i)- 5.0d0)**2.0_rDef + &
      0.0601194286579808d0)/(0.00100199047763301d0*tanh(10.0d0*r(i)- &
      10.0d0) + 0.00100199047763301d0*tanh(10.0d0*r(i)- 7.5d0) + &
      0.00100199047763301d0*tanh(10.0d0*r(i)- 5.0d0) + 1))
        axialMachData(i)      = 0.00833333333333333d0*tanh(125.0d0*r(i)- 125.0d0) + 0.00833333333333333d0* &
      tanh(125.0d0*r(i)- 100.0d0) + 0.00833333333333333d0*tanh(125.0d0*r(i)&
      - 75.0d0) + 0.00833333333333333d0*tanh(125.0d0*r(i)- 50.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r(i)- 25.0d0) + &
      0.166666666666667d0


        END DO

    END SUBROUTINE CalcSoundSpeed
