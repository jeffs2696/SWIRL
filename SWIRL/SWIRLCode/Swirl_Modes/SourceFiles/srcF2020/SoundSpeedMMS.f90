 
! Returns M_theta and the corresponding sound speed as defined in
! SourceTermSymbolicSolver.ipynb

    SUBROUTINE CalcSoundSpeed(& 
    r                 , &
    r2                , &
    r3                , &
    r_max             , &
    k, kappa          , &
    SoundSpeedExpected, &
    thetaMachData       &
    )
    
    REAL(KIND=rDef)   , INTENT(IN) :: &
    r2   , &
    r3   , &
    r_max, &
    kappa
    
    REAL(KIND=rDef)   , DIMENSION(:), INTENT(INOUT) :: &
    SoundSpeedExpected, thetaMachData
    
    REAL(KIND=rDef)   , DIMENSION(:), INTENT(IN) :: &
    r!, r_loc
    
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

        SoundSpeedExpected(i) = one + tanh((r(i)- r2)*kR(2))*kR(1) + tanh((r(i)- r3)*kR(2))*kR(1) + tanh((r(i)- &
      r_max)*kR(2))*kR(1) + tanh((r2 - r_max)*kR(2))*kR(1) + tanh((r3 - &
      r_max)*kR(2))*kR(1)
        thetaMachData(i)      = sqrt(r(i)*two*((1 - tanh((r(i)- r2)*kR(2))**2.0_rDef)*kR(1)*kR(2) + (1 - tanh((r(i)- r3)* &
      kR(2))**2.0_rDef)*kR(1)*kR(2) + (1 - tanh((r(i)- r_max)*kR(2))**2.0_rDef)*kR(1)*kR(2))/ &
      ((kappa - one)*(one + tanh((r(i)- r2)*kR(2))*kR(1) + tanh((r(i)- r3)*kR( &
      2))*kR(1) + tanh((r(i)- r_max)*kR(2))*kR(1) + tanh((r2 - r_max)*kR(2))* &
      kR(1) + tanh((r3 - r_max)*kR(2))*kR(1))))


        END DO

    END SUBROUTINE CalcSoundSpeed
