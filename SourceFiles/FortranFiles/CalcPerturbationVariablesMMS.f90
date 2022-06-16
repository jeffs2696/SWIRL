 
    
     
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
    vR(i) =-0.0*(-2.74348422496571d0*(r(i)  - 0.1d0)**3 + 3.7037037037037d0*(r(i)  - 0.1d0) &
      **2) - 0.0*(2.74348422496571d0*(r(i)  - 0.1d0)**3 - 3.7037037037037d0 &
      *(r(i)  - 0.1d0)**2 + 1) + 0.0
vTh(i)=0.0d0
vX(i) =0.0d0
Pr(i) =-0.00125659826432534d0*r(i)  - 2.48611347302937d0*(r(i)  - 0.1d0)**3 + &
      3.35000945713124d0*(r(i)  - 0.1d0)**2 + 0.100125659826433d0

          END DO
        END SUBROUTINE CalcPerturbationVariables
    