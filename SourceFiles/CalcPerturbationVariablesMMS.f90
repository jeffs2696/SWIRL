 

 
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
vTh(i)=0.0333333333333333d0*tanh(62.5d0*r(i) - 62.5d0) + 0.0333333333333333d0*tanh &
      (62.5d0*r(i) - 52.5d0) + 0.0333333333333333d0*tanh(62.5d0*r(i) - 42.5d0 &
      ) + 0.0333333333333333d0*tanh(62.5d0*r(i) - 32.499999999999993d0) + &
      0.0333333333333333d0*tanh(62.5d0*r(i) - 22.5d0) + &
      0.866666666804077d0
vX(i) =0.1d0*tanh(25.0d0*r(i) - 25.0d0) + 0.1d0*tanh(25.0d0*r(i) - &
      19.666666666666668d0) + 0.1d0*tanh(25.0d0*r(i) - &
      14.333333333333334d0) + 0.1d0*tanh(25.0d0*r(i) - 9.0d0) + &
      0.700004661820234d0
Pr(i) =-1.77635683940025d-15*r(i) - 6.24081214212538d0*(r(i) - 0.2d0)**3 + &
      4.9926497137003d0*(r(i) - 0.2d0)**2 + 0.0416666666666667d0*tanh( &
      125.0d0*r(i) - 125.0d0) + 0.0416666666666667d0*tanh(125.0d0*r(i) - &
      105.0d0) + 0.0416666666666667d0*tanh(125.0d0*r(i) - 85.0d0) + &
      0.0416666666666667d0*tanh(125.0d0*r(i) - 64.999999999999986d0) + &
      0.0416666666666667d0*tanh(125.0d0*r(i) - 45.0d0) + &
      0.833333333333334d0

      END DO
    END SUBROUTINE CalcPerturbationVariables
