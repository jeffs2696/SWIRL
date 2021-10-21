 
! Returns M_theta and the corresponding sound speed as defined in
! SourceTermSymbolicSolver.ipynb

    SUBROUTINE CalcSoundSpeed(& 
    r                 , &
    r_max             , &
    k, kappa          , &
    SoundSpeedExpected, &
    thetaMachData     , &
    axialMachData)
    
    REAL(KIND=rDef)   , INTENT(IN) :: &
    r_max, &
    kappa
    
    REAL(KIND=rDef)   , DIMENSION(:), INTENT(INOUT) :: &
    SoundSpeedExpected, thetaMachData, axialMachData
    
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

        SoundSpeedExpected(i) = 0.333333333333073d0*r(i)**3 - 0.499999999999609d0*r(i)**2 - (-2*r(i)**3 + 3*r(i)**2) &
      *(0.0833333333333333d0*1.0d0 + 0.416666666666667d0) + &
      0.0833333333333333d0*tanh(100.0d0*r) + 0.0833333333333333d0*tanh( &
      100.0d0*r(i)- 100.0d0) + 0.0833333333333333d0*tanh(100.0d0*r(i)- &
      85.714285714285722d0) + 0.0833333333333333d0*tanh(100.0d0*r(i)- &
      71.428571428571431d0) + 0.0833333333333333d0*tanh(100.0d0*r(i)- &
      57.142857142857139d0) + 0.0833333333333333d0*tanh(100.0d0*r(i)- &
      42.857142857142861d0) + 0.0833333333333333d0*tanh(100.0d0*r(i)- &
      28.57142857142858d0) + 0.0833333333333333d0*tanh(100.0d0*r(i)- &
      14.28571428571429d0) + 0.583333333333268d0
        thetaMachData(i)      = sqrt(r(i)*two*(0.999999999999218d0*r(i)**2.0_rDef - 0.999999999999218d0*r(i)+ ( &
      -0.416666666666667d0 - 0.0833333333333333d0*1.0d0)*(-6*r(i)**2.0_rDef + 6*r(i)&
      ) - 8.33333333333333d0*tanh(100.0d0*r)**2.0_rDef - 8.33333333333333d0* &
      tanh(100.0d0*r(i)- 100.0d0)**2.0_rDef - 8.33333333333333d0*tanh(100.0d0*r(i)&
      - 85.714285714285722d0)**2.0_rDef - 8.33333333333333d0*tanh(100.0d0*r(i)- &
      71.428571428571431d0)**2.0_rDef - 8.33333333333333d0*tanh(100.0d0*r(i)- &
      57.142857142857139d0)**2.0_rDef - 8.33333333333333d0*tanh(100.0d0*r(i)- &
      42.857142857142861d0)**2.0_rDef - 8.33333333333333d0*tanh(100.0d0*r(i)- &
      28.57142857142858d0)**2.0_rDef - 8.33333333333333d0*tanh(100.0d0*r(i)- &
      14.28571428571429d0)**2.0_rDef + 66.6666666666666d0)/((kappa - one)*( &
      0.333333333333073d0*r(i)**3 - 0.499999999999609d0*r(i)**2.0_rDef - (-2*r(i)**3 + &
      3*r(i)**2.0_rDef)*(0.0833333333333333d0*1.0d0 + 0.416666666666667d0) + &
      0.0833333333333333d0*tanh(100.0d0*r) + 0.0833333333333333d0*tanh( &
      100.0d0*r(i)- 100.0d0) + 0.0833333333333333d0*tanh(100.0d0*r(i)- &
      85.714285714285722d0) + 0.0833333333333333d0*tanh(100.0d0*r(i)- &
      71.428571428571431d0) + 0.0833333333333333d0*tanh(100.0d0*r(i)- &
      57.142857142857139d0) + 0.0833333333333333d0*tanh(100.0d0*r(i)- &
      42.857142857142861d0) + 0.0833333333333333d0*tanh(100.0d0*r(i)- &
      28.57142857142858d0) + 0.0833333333333333d0*tanh(100.0d0*r(i)- &
      14.28571428571429d0) + 0.583333333333268d0)))
        axialMachData(i)      = sin((r(i)- 1.0d0)*kR(3))


        END DO

    END SUBROUTINE CalcSoundSpeed
