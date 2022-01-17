 

 
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
vTh(i)=0.25d0*tanh(62.5d0*r(i) - 62.5d0) + 0.25d0*tanh(62.5d0*r(i) - 12.5d0) + 0.75d0
vX(i) =0.15d0*tanh(25.0d0*r(i) - 25.0d0) + 0.15d0*tanh(25.0d0*r(i) - &
      18.333333333333336d0) + 0.15d0*tanh(25.0d0*r(i) - &
      11.666666666666668d0) + 0.15d0*tanh(25.0d0*r(i) - 5.0d0) + &
      0.550000485879038d0
Pr(i) =-3.07514231714424d0*r(i) - 9.01174525386818d0*(r(i) - 0.2d0)**3 + &
      11.0533240995248d0*(r(i) - 0.2d0)**2 + 0.25d0*tanh(12.5d0*r(i) - 12.5d0 &
      ) + 0.25d0*tanh(12.5d0*r(i) - 2.5d0) + 1.36502846445942d0

      END DO
    END SUBROUTINE CalcPerturbationVariables
