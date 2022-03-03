 
    
     
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
    vR(i) =-0.0*(-2.0d0*r(i) **3 + 3.0d0*r(i) **2) - 0.0*(2.0d0*r(i) **3 - 3.0d0*r(i) **2 + 1) + &
      0.0
vTh(i)=0
vX(i) =0
Pr(i) =-2.0d0*r(i) **3 + 3.0d0*r(i) **2

          END DO
        END SUBROUTINE CalcPerturbationVariables
    