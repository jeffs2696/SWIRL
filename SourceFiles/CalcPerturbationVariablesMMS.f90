 

 
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
vR(i) =6.27790178571429d0*(r(i) - 0.2d0)**3 - 7.53348214285715d0*(r(i) - 0.2d0)**2 + &
      0.0357142857142857d0*tanh(125.0d0*r(i) - 125.0d0) + &
      0.0357142857142857d0*tanh(125.0d0*r(i) - 108.99999999999999d0) + &
      0.0357142857142857d0*tanh(125.0d0*r(i) - 93.0d0) + &
      0.0357142857142857d0*tanh(125.0d0*r(i) - 77.0d0) + &
      0.0357142857142857d0*tanh(125.0d0*r(i) - 61.0d0) + &
      0.0357142857142857d0*tanh(125.0d0*r(i) - 45.0d0) + &
      1.42857142857143d0
vTh(i)=0.0666666666666667d0*tanh(62.5d0*r(i) - 62.5d0) + 0.0666666666666667d0*tanh &
      (62.5d0*r(i) - 22.5d0) + 0.933333333333333d0
vX(i) =0.1d0*tanh(25.0d0*r(i) - 25.0d0) + 0.1d0*tanh(25.0d0*r(i) - &
      19.666666666666668d0) + 0.1d0*tanh(25.0d0*r(i) - &
      14.333333333333334d0) + 0.1d0*tanh(25.0d0*r(i) - 9.0d0) + &
      0.700004661820234d0
Pr(i) =0.0232310199824084d0*r(i) - 1.56637123339521d0*(r(i) - 0.2d0)**3 + &
      1.22405821173816d0*(r(i) - 0.2d0)**2 + 0.116666666666667d0*tanh( &
      12.5d0*r(i) - 12.5d0) + 0.116666666666667d0*tanh(12.5d0*r(i) - 4.5d0) + &
      0.878687155595056d0

      END DO
    END SUBROUTINE CalcPerturbationVariables
