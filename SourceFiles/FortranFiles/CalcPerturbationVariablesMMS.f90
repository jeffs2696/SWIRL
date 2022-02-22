 

 
    SUBROUTINE CalcPerturbationVariables(& 
    r    , &
    vR   , &
    vTh  , &
    vX   , &
    Pr)
    

    REAL(KIND=rDef)  ,DIMENSION(:), INTENT(IN)    :: r
    REAL(KIND=rDef)  ,DIMENSION(:), INTENT(INOUT) :: vR, vTh, vX, Pr

    
    ! Local variables
    INTEGER :: numberOfGridPoints, i
    
    numberOfGridPoints = SIZE(vR)
      DO i = 1, numberOfGridPoints
vR(i) =2.05447160938161d0*(r(i) - 0.2d0)**3 - 2.46536593125793d0*(r(i) - 0.2d0)**2 + &
      0.125d0*tanh(1.25d0*r(i) - 1.25d0) + 0.125d0*tanh(1.25d0*r(i) - 1.0d0) &
      + 0.125d0*tanh(1.25d0*r(i) - 0.75d0) + 0.125d0*tanh(1.25d0*r(i) - 0.5d0 &
      ) + 0.125d0*tanh(1.25d0*r(i) - 0.25d0) + 0.262972366000846d0
vTh(i)=0.125d0*tanh(25.0d0*r(i) - 25.0d0) + 0.125d0*tanh(25.0d0*r(i) - 20.0d0) + &
      0.125d0*tanh(25.0d0*r(i) - 15.0d0) + 0.125d0*tanh(25.0d0*r(i) - 10.0d0 &
      ) + 0.125d0*tanh(25.0d0*r(i) - 5.0d0) + 0.500011349982487d0
vX(i) =0.125d0*tanh(25.0d0*r(i) - 25.0d0) + 0.125d0*tanh(25.0d0*r(i) - 20.0d0) + &
      0.125d0*tanh(25.0d0*r(i) - 15.0d0) + 0.125d0*tanh(25.0d0*r(i) - 10.0d0 &
      ) + 0.125d0*tanh(25.0d0*r(i) - 5.0d0) + 0.500011349982487d0
Pr(i) =-1.60433980646514d0*r(i) - 4.69921713977753d0*(r(i) - 0.2d0)**3 + &
      5.76479846990345d0*(r(i) - 0.2d0)**2 + 0.125d0*tanh(12.5d0*r(i) - &
      12.5d0) + 0.125d0*tanh(12.5d0*r(i) - 10.0d0) + 0.125d0*tanh(12.5d0*r(i) &
      - 7.5d0) + 0.125d0*tanh(12.5d0*r(i) - 5.0d0) + 0.125d0*tanh(12.5d0*r(i) &
      - 2.5d0) + 0.82255260048212d0

      END DO
    END SUBROUTINE CalcPerturbationVariables
