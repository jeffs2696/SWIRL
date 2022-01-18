 
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

        SoundSpeedExpected(i) = 0.001d0*tanh(10.0d0*r(i)- 10.0d0) + 0.001d0*tanh(10.0d0*r(i)- 7.5d0) + &
      0.001d0*tanh(10.0d0*r(i)- 5.0d0) + 0.998013476497586d0
        thetaMachData(i)      = 1.58113883008419d0*sqrt(r(i)*(-0.0200398095526603d0*tanh(10.0d0*r(i)- 10.0d0) &
      **2 - 0.0200398095526603d0*tanh(10.0d0*r(i)- 7.5d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r(i)- 5.0d0)**2 + &
      0.0601194286579808d0)/(0.00100199047763301d0*tanh(10.0d0*r(i)- &
      10.0d0) + 0.00100199047763301d0*tanh(10.0d0*r(i)- 7.5d0) + &
      0.00100199047763301d0*tanh(10.0d0*r(i)- 5.0d0) + 1))
        axialMachData(i)      = 0.1125d0*tanh(12.5d0*r(i)- 12.5d0) + 0.3d0


        END DO

    END SUBROUTINE CalcSoundSpeed
