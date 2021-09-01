 
    SUBROUTINE CalcSoundSpeed(& 
    r                 , &
    r_max             , &
    k, kappa          , &
    SoundSpeedExpected, &
    thetaMachData       &
    )
    
    REAL(KIND=rDef)   , INTENT(IN) :: &
    r_max,kappa
    
    REAL(KIND=rDef)   , DIMENSION(:), INTENT(INOUT) :: &
    SoundSpeedExpected, thetaMachData
    
    REAL(KIND=rDef)   , DIMENSION(:), INTENT(IN) :: &
    r
    
    COMPLEX(KIND=rDef), DIMENSION(:), INTENT(IN) :: &
    k
     
    ! Local variables 
    INTEGER :: &
    numberOfGridPoints, i
    
    REAL(KIND = rDef) :: one,two,three
    
    REAL(KIND=rDef), DIMENSION(SIZE(k)) :: kR
    
    kR = REAL(k,KIND = rDef)
        
    one   = (1.0_rDef)    
    two   = (2.0_rDef)    
    three = (3.0_rDef)

    
    numberOfGridPoints = SIZE(SoundSpeedExpected)

        DO i = 1,numberOfGridPoints

        SoundSpeedExpected(i) = one - tanh((one - r_max)*kR(2))*kR(1) + tanh((r(i)- r_max)*kR(2))*kR(1)
        thetaMachData(i)      = sqrt(r(i)*two*kR(1)*kR(2)/((kappa - one)*(one - tanh((one - r_max)*kR(2))*kR(1 &
      ) + tanh((r(i)- r_max)*kR(2))*kR(1))*cosh((r(i)- r_max)*kR(2))**2.0_rDef))


        END DO

    END SUBROUTINE CalcSoundSpeed
