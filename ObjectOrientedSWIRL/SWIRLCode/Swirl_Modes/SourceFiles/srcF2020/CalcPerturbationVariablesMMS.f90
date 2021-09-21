 

 
    SUBROUTINE CalcPerturbationVariables(& 
    k    , &
    r    , &
    r_max, &
    vR   , &
    vTh  , &
    vX   , &
    Pr)
    

    REAL(KIND=rDef)  , INTENT(IN) :: r_max
    REAL(KIND=rDef)  ,DIMENSION(:), INTENT(IN)    :: r
    REAL(KIND=rDef)  ,DIMENSION(:), INTENT(INOUT) :: vR, vTh, vX, Pr
    COMPLEX(KIND=rDef), DIMENSION(:), INTENT(IN) :: k
    
    ! Local variables
    INTEGER :: numberOfGridPoints, i
    
    numberOfGridPoints = SIZE(vR)
      DO i = 1, numberOfGridPoints
vR(i) =cos((r(i) - r_max)*k(4))*k(4)
vTh(i)=cos((r(i) - r_max)*k(5))*k(5)
vX(i) =cos((r(i) - r_max)*k(6))*k(6)
Pr(i) =cos((r(i) - r_max)*k(7))*k(7)

      END DO
    END SUBROUTINE CalcPerturbationVariables
