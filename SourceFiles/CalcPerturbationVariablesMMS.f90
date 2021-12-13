 

 
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
vR(i) =0.875d0*r(i) + 2.74658203125d0*(r(i) - 0.2d0)**5 - 3.41796875d0*(r(i) - 0.2d0)**4 &
      - 0.390624999999998d0*(r(i) - 0.2d0)**3 - 6.25d0*tanh(125.0d0*r(i) - &
      125.0d0)**2 - 6.25d0*tanh(125.0d0*r(i) - 91.666666666666671d0)**2 - &
      6.25d0*tanh(125.0d0*r(i) - 58.333333333333336d0)**2 - 6.25d0*tanh( &
      125.0d0*r(i) - 25.0d0)**2 + 24.825d0
vTh(i)=0.0333333333333333d0*tanh(18.75d0*r(i) - 18.75d0) + 0.0333333333333333d0* &
      tanh(18.75d0*r(i) - 15.0d0) + 0.0333333333333333d0*tanh(18.75d0*r(i) - &
      11.25d0) + 0.0333333333333333d0*tanh(18.75d0*r(i) - 7.5d0) + &
      0.0333333333333333d0*tanh(18.75d0*r(i) - 3.75d0) + &
      0.866703538980562d0
vX(i) =0.1d0*tanh(25.0d0*r(i) - 25.0d0) + 0.1d0*tanh(25.0d0*r(i) - &
      18.333333333333336d0) + 0.1d0*tanh(25.0d0*r(i) - &
      11.666666666666668d0) + 0.1d0*tanh(25.0d0*r(i) - 5.0d0) + &
      0.700000323919358d0
Pr(i) =0.875d0*r(i) + 2.74658203125d0*(r(i) - 0.2d0)**5 - 3.41796875d0*(r(i) - 0.2d0)**4 &
      - 0.390624999999998d0*(r(i) - 0.2d0)**3 - 6.25d0*tanh(125.0d0*r(i) - &
      125.0d0)**2 - 6.25d0*tanh(125.0d0*r(i) - 91.666666666666671d0)**2 - &
      6.25d0*tanh(125.0d0*r(i) - 58.333333333333336d0)**2 - 6.25d0*tanh( &
      125.0d0*r(i) - 25.0d0)**2 + 24.825d0

      END DO
    END SUBROUTINE CalcPerturbationVariables
