 

 
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
vR(i) =1.171875d0*(r(i) - 0.2d0)**3 - 1.40625d0*(r(i) - 0.2d0)**2 + 0.05d0*tanh( &
      125.0d0*r(i) - 125.0d0) + 0.05d0*tanh(125.0d0*r(i) - &
      91.666666666666671d0) + 0.05d0*tanh(125.0d0*r(i) - &
      58.333333333333336d0) + 0.05d0*tanh(125.0d0*r(i) - 25.0d0) + 0.15d0
vTh(i)=0.0333333333333333d0*tanh(18.75d0*r(i) - 18.75d0) + 0.0333333333333333d0* &
      tanh(18.75d0*r(i) - 15.0d0) + 0.0333333333333333d0*tanh(18.75d0*r(i) - &
      11.25d0) + 0.0333333333333333d0*tanh(18.75d0*r(i) - 7.5d0) + &
      0.0333333333333333d0*tanh(18.75d0*r(i) - 3.75d0) + &
      0.866703538980562d0
vX(i) =0.1d0*tanh(25.0d0*r(i) - 25.0d0) + 0.1d0*tanh(25.0d0*r(i) - &
      18.333333333333336d0) + 0.1d0*tanh(25.0d0*r(i) - &
      11.666666666666668d0) + 0.1d0*tanh(25.0d0*r(i) - 5.0d0) + &
      0.700000323919358d0
Pr(i) =0.01875d0*tanh(62.5d0*r(i) - 62.5d0) + 0.01875d0*tanh(62.5d0*r(i) - &
      58.928571428571431d0) + 0.01875d0*tanh(62.5d0*r(i) - &
      55.357142857142861d0) + 0.01875d0*tanh(62.5d0*r(i) - &
      51.785714285714278d0) + 0.01875d0*tanh(62.5d0*r(i) - &
      48.214285714285715d0) + 0.01875d0*tanh(62.5d0*r(i) - &
      44.642857142857146d0) + 0.01875d0*tanh(62.5d0*r(i) - &
      41.071428571428569d0) + 0.01875d0*tanh(62.5d0*r(i) - 37.5d0) + &
      0.01875d0*tanh(62.5d0*r(i) - 33.928571428571431d0) + 0.01875d0*tanh( &
      62.5d0*r(i) - 30.357142857142861d0) + 0.01875d0*tanh(62.5d0*r(i) - &
      26.785714285714292d0) + 0.01875d0*tanh(62.5d0*r(i) - &
      23.214285714285715d0) + 0.01875d0*tanh(62.5d0*r(i) - &
      19.642857142857146d0) + 0.01875d0*tanh(62.5d0*r(i) - &
      16.071428571428577d0) + 0.01875d0*tanh(62.5d0*r(i) - 12.5d0) + &
      0.737529643424149d0

      END DO
    END SUBROUTINE CalcPerturbationVariables
