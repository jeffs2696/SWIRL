 

 
    SUBROUTINE CalcPerturbationVariables(& 
    k    , &
    r    , &
    vR   , &
    vTh  , &
    vX   , &
    Pr)
    

    REAL(KIND=rDef)  ,DIMENSION(:), INTENT(IN)    :: r
    REAL(KIND=rDef)  ,DIMENSION(:), INTENT(INOUT) :: vR, vTh, vX, Pr
    COMPLEX(KIND=rDef), DIMENSION(:), INTENT(IN) :: k
    
    ! Local variables
    INTEGER :: numberOfGridPoints, i
    
    numberOfGridPoints = SIZE(vR)
      DO i = 1, numberOfGridPoints
vR(i) =1.51909722222205d0*(r(i) - 0.2d0)**3 - 1.82291666666646d0*(r(i) - 0.2d0)**2 + &
      0.0277777777777778d0*tanh(125.0d0*r(i) - 125.0d0) + &
      0.0277777777777778d0*tanh(125.0d0*r(i) - 110.71428571428572d0) + &
      0.0277777777777778d0*tanh(125.0d0*r(i) - 96.428571428571431d0) + &
      0.0277777777777778d0*tanh(125.0d0*r(i) - 82.142857142857139d0) + &
      0.0277777777777778d0*tanh(125.0d0*r(i) - 67.857142857142861d0) + &
      0.0277777777777778d0*tanh(125.0d0*r(i) - 53.571428571428584d0) + &
      0.0277777777777778d0*tanh(125.0d0*r(i) - 39.285714285714292d0) + &
      0.0277777777777778d0*tanh(125.0d0*r(i) - 25.0d0) + &
      0.194444444444423d0
vTh(i)=r(i)*k(5)
vX(i) =r(i)*k(6)
Pr(i) =r(i)

      END DO
    END SUBROUTINE CalcPerturbationVariables
