 
    
     
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
    vR(i) =0.0102849805345748d0*(r(i)  - 0.1d0)**3 - 0.0138847237216759d0*(r(i)  - 0.1d0)** &
      2 + 0.125d0*tanh(0.033333333333333333d0*r(i)  - &
      0.033333333333333333d0) + 0.00374887540485247d0
vTh(i)=0.125d0*tanh(0.033333333333333333d0*r(i)  - 0.033333333333333333d0) + 1
vX(i) =0.125d0*tanh(0.033333333333333333d0*r(i)  - 0.033333333333333333d0) + 1
Pr(i) =0.89825957854535d0*r(i)  + 1.44991939893847d0*(r(i)  - 0.1d0)**3 - &
      2.30299365742835d0*(r(i)  - 0.1d0)**2 + 0.125d0*tanh( &
      0.033333333333333333d0*r(i)  - 0.033333333333333333d0) + &
      0.910174042145465d0

          END DO
        END SUBROUTINE CalcPerturbationVariables
    