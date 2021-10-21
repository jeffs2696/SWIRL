 

 
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
vR(i) =4.8828125d0*(r(i) - 0.2d0)**3 - 5.859375d0*(r(i) - 0.2d0)**2 + 0.1875d0*tanh( &
      125.0d0*r(i) - 125.0d0) + 0.1875d0*tanh(125.0d0*r(i) - 75.0d0) + &
      0.1875d0*tanh(125.0d0*r(i) - 25.0d0) + 0.875d0
vTh(i)=sin((r(i) - 1.0d0)*k(5))
vX(i) =sin((r(i) - 1.0d0)*k(6))
Pr(i) =4.8828125d0*(r(i) - 0.2d0)**3 - 5.859375d0*(r(i) - 0.2d0)**2 + 0.1875d0*tanh( &
      125.0d0*r(i) - 125.0d0) + 0.1875d0*tanh(125.0d0*r(i) - 75.0d0) + &
      0.1875d0*tanh(125.0d0*r(i) - 25.0d0) + 0.875d0

      END DO
    END SUBROUTINE CalcPerturbationVariables
