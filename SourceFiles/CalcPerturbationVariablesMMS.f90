 

 
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
vR(i) =1.53459821428571d0*(r(i) - 0.2d0)**3 - 1.84151785714285d0*(r(i) - 0.2d0)**2 + &
      0.0357142857142857d0*tanh(125.0d0*r(i) - 125.0d0) + &
      0.0357142857142857d0*tanh(125.0d0*r(i) - 108.99999999999999d0) + &
      0.0357142857142857d0*tanh(125.0d0*r(i) - 93.0d0) + &
      0.0357142857142857d0*tanh(125.0d0*r(i) - 77.0d0) + &
      0.0357142857142857d0*tanh(125.0d0*r(i) - 61.0d0) + &
      0.0357142857142857d0*tanh(125.0d0*r(i) - 45.0d0) + &
      0.214285714285714d0
vTh(i)=0.0666666666666667d0*tanh(62.5d0*r(i) - 62.5d0) + 0.0666666666666667d0*tanh &
      (62.5d0*r(i) - 22.5d0) + 0.933333333333333d0
vX(i) =0.1d0*tanh(25.0d0*r(i) - 25.0d0) + 0.1d0*tanh(25.0d0*r(i) - &
      19.666666666666668d0) + 0.1d0*tanh(25.0d0*r(i) - &
      14.333333333333334d0) + 0.1d0*tanh(25.0d0*r(i) - 9.0d0) + &
      0.700004661820234d0
Pr(i) =-0.0232310199824084d0*r(i) - 1.63896817084024d0*(r(i) - 0.2d0)**3 + &
      1.3402133116502d0*(r(i) - 0.2d0)**2 + 0.116666666666667d0*tanh( &
      12.5d0*r(i) - 12.5d0) + 0.116666666666667d0*tanh(12.5d0*r(i) - 4.5d0) + &
      0.887979563588019d0

      END DO
    END SUBROUTINE CalcPerturbationVariables
