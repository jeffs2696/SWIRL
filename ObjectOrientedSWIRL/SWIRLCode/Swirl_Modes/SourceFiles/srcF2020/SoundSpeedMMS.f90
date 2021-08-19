 
    SUBROUTINE CalcSoundSpeed(& 
    r  , &
    r_max , &
    k, kappa, SoundSpeedExpected,thetaMachData       &
    )
    
    REAL(KIND=rDef)   , INTENT(IN) :: &
    r_max,kappa
    
    REAL(KIND=rDef), DIMENSION(:), INTENT(INOUT) :: &
    SoundSpeedExpected, thetaMachData
    
    REAL(KIND=rDef), DIMENSION(:), INTENT(IN) :: r
    
    COMPLEX(KIND=rDef), DIMENSION(:), INTENT(IN) :: k
     
    INTEGER :: numberOfGridPoints, i
    
    numberOfGridPoints = SIZE(SoundSpeedExpected)

        DO i = 1,numberOfGridPoints
        SoundSpeedExpected(i) = tanh((r(i)- r_max)*k(2))*k(1) + 1.0d0
        thetaMachData(i)      = 1.4142135623731d0*sqrt(r(i)*(1 - tanh((r(i)- r_max)*k(2))**2)*1d0/(tanh((r(i)- &
      r_max)*k(2))*k(1) + 1.0d0)*k(1)*k(2)/(kappa - 1.0d0))

        END DO

    END SUBROUTINE CalcSoundSpeed
