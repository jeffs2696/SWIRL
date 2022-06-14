 
    
     
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
    vR(i) =0.261177694086339d0*(r(i)  - 0.1d0)**3 - 0.352589887016558d0*(r(i)  - 0.1d0)**2 &
      + 0.125d0*tanh(1.1111111111111112d0*r(i)  - 1.1111111111111112d0) + &
      0.0951992694944706d0
vTh(i)=0.125d0*tanh(2.2222222222222223d0*r(i)  - 2.2222222222222223d0) + 1
vX(i) =0.125d0*tanh(2.2222222222222223d0*r(i)  - 2.2222222222222223d0) + 1
Pr(i) =0.738122828813086d0*r(i)  + 1.08589597677244d0*(r(i)  - 0.1d0)**3 - &
      1.79744285555418d0*(r(i)  - 0.1d0)**2 + 0.125d0*tanh( &
      1.1111111111111112d0*r(i)  - 1.1111111111111112d0) + &
      0.926187717118691d0

          END DO
        END SUBROUTINE CalcPerturbationVariables
    