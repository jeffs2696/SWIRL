 

 
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
vR(i) =1.95312499194862d0*(r(i) - 0.2d0)**3 - 2.34374999033834d0*(r(i) - 0.2d0)**2 + &
      0.25d0*tanh(12.5d0*r(i) - 12.5d0) + 0.25d0*tanh(12.5d0*r(i) - 2.5d0) + &
      0.249999998969423d0
vTh(i)=0
vX(i) =0
Pr(i) =-3.12509985599234d0*r(i) - 9.79786774680476d0*(r(i) - 0.2d0)**3 + &
      11.7446690174342d0*(r(i) - 0.2d0)**2 + 0.25d0*tanh(12.5d0*r(i) - 12.5d0 &
      ) + 0.25d0*tanh(12.5d0*r(i) - 2.5d0) + 1.37501997222905d0

      END DO
    END SUBROUTINE CalcPerturbationVariables
