 
    
     
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
Pr(i) =-0.000835716781190321d0*r(i)  - 2.48045561742534d0*(r(i)  - 0.1d0)**3 + &
      3.34444974099524d0*(r(i)  - 0.1d0)**2 + 0.100083571678119d0

          END DO
        END SUBROUTINE CalcPerturbationVariables
    