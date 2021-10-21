 
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

        SoundSpeedExpected(i) = 0.1d0*tanh(0.10000000000000001d0*r(i)- 0.10000000000000001d0) + 1
        thetaMachData(i)      = sqrt(r(i)*two*(0.01d0 - 0.01d0*tanh(0.10000000000000001d0*r(i)- &
      0.10000000000000001d0)**2.0_rDef)/((kappa - one)*(0.1d0*tanh( &
      0.10000000000000001d0*r(i)- 0.10000000000000001d0) + 1)))
        axialMachData(i)      = r(i)- 1


        END DO

    END SUBROUTINE CalcSoundSpeed
