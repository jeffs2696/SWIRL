 
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
    
        
        numberOfGridPoints = SIZE(SoundSpeedExpected)
    
            DO i = 1,numberOfGridPoints
    
            SoundSpeedExpected(i) = 0.1875d0*tanh(0.0033333333333333335d0*r(i)- 0.0033333333333333335d0) + &
      0.1875d0*tanh(0.0033333333333333335d0*r(i)- 0.0018333333333333335d0 &
      ) + 0.1875d0*tanh(0.0033333333333333335d0*r(i)- &
      0.00033333333333333338d0) + 0.999156251898431d0
        thetaMachData(i)      = 1.58113883008419d0*sqrt(r(i)*1d0/(0.187658336365052d0*tanh( &
      0.0033333333333333335d0*r(i)- 0.0033333333333333335d0) + &
      0.187658336365052d0*tanh(0.0033333333333333335d0*r(i)- &
      0.0018333333333333335d0) + 0.187658336365052d0*tanh( &
      0.0033333333333333335d0*r(i)- 0.00033333333333333338d0) + 1)*( &
      -0.00125105557576701d0*tanh(0.0033333333333333335d0*r(i)- &
      0.0033333333333333335d0)**2 - 0.00125105557576701d0*tanh( &
      0.0033333333333333335d0*r(i)- 0.0018333333333333335d0)**2 - &
      0.00125105557576701d0*tanh(0.0033333333333333335d0*r(i)- &
      0.00033333333333333338d0)**2 + 0.00375316672730103d0))
        axialMachData(i)      = 0.0125d0*tanh(55.555555555555557d0*r(i)- 55.555555555555557d0) + 0.0125d0* &
      tanh(55.555555555555557d0*r(i)- 43.055555555555557d0) + 0.0125d0* &
      tanh(55.555555555555557d0*r(i)- 30.555555555555557d0) + 0.0125d0* &
      tanh(55.555555555555557d0*r(i)- 18.055555555555554d0) + 0.0125d0* &
      tanh(55.555555555555557d0*r(i)- 5.5555555555555562d0) + &
      0.0500000000003472d0

    
            END DO
    
        END SUBROUTINE CalcSoundSpeed
    