 

 
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
vR(i) =2.929554492393d0*(r(i) - 0.2d0)**3 - 3.5154653908716d0*(r(i) - 0.2d0)**2 + &
      0.1875d0*tanh(12.5d0*r(i) - 12.5d0) + 0.1875d0*tanh(12.5d0*r(i) - 7.5d0 &
      ) + 0.1875d0*tanh(12.5d0*r(i) - 2.5d0) + 0.374982975026304d0
vTh(i)=0.25d0*tanh(250.0d0*r(i) - 250.0d0) + 0.25d0*tanh(250.0d0*r(i) - 50.0d0) + &
      0.75d0
vX(i) =0.15d0*tanh(250.0d0*r(i) - 250.0d0) + 0.15d0*tanh(250.0d0*r(i) - &
      183.33333333333334d0) + 0.15d0*tanh(250.0d0*r(i) - &
      116.66666666666667d0) + 0.15d0*tanh(250.0d0*r(i) - 50.0d0) + 0.55d0
Pr(i) =-7.39206186872954d0*r(i) - 23.1931743678093d0*(r(i) - 0.2d0)**3 + &
      27.7946168301593d0*(r(i) - 0.2d0)**2 + 0.125d0*tanh(62.5d0*r(i) - &
      62.5d0) + 0.125d0*tanh(62.5d0*r(i) - 50.0d0) + 0.125d0*tanh(62.5d0*r(i) &
      - 37.5d0) + 0.125d0*tanh(62.5d0*r(i) - 25.0d0) + 0.125d0*tanh(62.5d0 &
      *r(i) - 12.5d0) + 1.97841237374938d0

      END DO
    END SUBROUTINE CalcPerturbationVariables
