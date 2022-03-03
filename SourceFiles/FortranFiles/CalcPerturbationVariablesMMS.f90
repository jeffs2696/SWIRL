 
    
     
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
    vR(i) =0.783533082259018d0*(r(i)  - 0.1d0)**3 - 1.05776966104967d0*(r(i)  - 0.1d0)**2 + &
      0.375d0*tanh(1.1111111111111112d0*r(i)  - 1.1111111111111112d0) + &
      0.285597808483412d0
vTh(i)=0
vX(i) =0
Pr(i) =-8.33645116815607d-5*r(i)  - 2.47026752820372d0*(r(i)  - 0.1d0)**3 + &
      3.33444451372966d0*(r(i)  - 0.1d0)**2 + 0.100008336451168d0

          END DO
        END SUBROUTINE CalcPerturbationVariables
    