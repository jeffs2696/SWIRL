 
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

        SoundSpeedExpected(i) = 0.1875d0*tanh(0.012500000000000001d0*r(i)- 0.012500000000000001d0) + &
      0.1875d0*tanh(0.012500000000000001d0*r(i)- 0.0074999999999999997d0 &
      ) + 0.1875d0*tanh(0.012500000000000001d0*r(i)- &
      0.0025000000000000001d0) + 0.997187570309922d0
        thetaMachData(i)      = 1.58113883008419d0*sqrt(r(i)*(-0.00470072044574637d0*tanh( &
      0.012500000000000001d0*r(i)- 0.012500000000000001d0)**2 - &
      0.00470072044574637d0*tanh(0.012500000000000001d0*r(i)- &
      0.0074999999999999997d0)**2 - 0.00470072044574637d0*tanh( &
      0.012500000000000001d0*r(i)- 0.0025000000000000001d0)**2 + &
      0.0141021613372391d0)/(0.188028817829855d0*tanh( &
      0.012500000000000001d0*r(i)- 0.012500000000000001d0) + &
      0.188028817829855d0*tanh(0.012500000000000001d0*r(i)- &
      0.0074999999999999997d0) + 0.188028817829855d0*tanh( &
      0.012500000000000001d0*r(i)- 0.0025000000000000001d0) + 1))
        axialMachData(i)      = 0.0375d0*tanh(12.5d0*r(i)- 12.5d0) + 0.0375d0*tanh(12.5d0*r(i)- 10.0d0) + &
      0.0375d0*tanh(12.5d0*r(i)- 7.5d0) + 0.0375d0*tanh(12.5d0*r(i)- 5.0d0 &
      ) + 0.0375d0*tanh(12.5d0*r(i)- 2.5d0) + 0.150505391756728d0


        END DO

    END SUBROUTINE CalcSoundSpeed
