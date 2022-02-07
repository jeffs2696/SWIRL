 

 
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
vR(i) =1.05188949556007d0*(r(i) - 1.0d-8)**3 - 1.57783422756176d0*(r(i) - 1.0d-8)**2 &
      + 0.125d0*tanh(1.0000000100000002d0*r(i) - 1.0000000100000002d0) + &
      0.125d0*tanh(1.0000000100000002d0*r(i) - 0.75000001000000005d0) + &
      0.125d0*tanh(1.0000000100000002d0*r(i) - 0.50000001000000005d0) + &
      0.125d0*tanh(1.0000000100000002d0*r(i) - 0.25000001000000011d0) + &
      0.125d0*tanh(1.0000000100000002d0*r(i) - 1.0000000100000002d-8) + &
      0.262972366000846d0
vTh(i)=0.125d0*tanh(20.000000200000002d0*r(i) - 20.000000200000002d0) + 0.125d0* &
      tanh(20.000000200000002d0*r(i) - 15.000000200000001d0) + 0.125d0* &
      tanh(20.000000200000002d0*r(i) - 10.000000200000001d0) + 0.125d0* &
      tanh(20.000000200000002d0*r(i) - 5.0000002000000023d0) + 0.125d0* &
      tanh(20.000000200000002d0*r(i) - 2.0000000200000005d-7) + &
      0.500011349982487d0
vX(i) =0.125d0*tanh(20.000000200000002d0*r(i) - 20.000000200000002d0) + 0.125d0* &
      tanh(20.000000200000002d0*r(i) - 15.000000200000001d0) + 0.125d0* &
      tanh(20.000000200000002d0*r(i) - 10.000000200000001d0) + 0.125d0* &
      tanh(20.000000200000002d0*r(i) - 5.0000002000000023d0) + 0.125d0* &
      tanh(20.000000200000002d0*r(i) - 2.0000000200000005d-7) + &
      0.500011349982487d0
Pr(i) =-0.197982768915281d0*r(i) + 0.915369119287634d0*(r(i) - 1.0d-8)**3 - &
      0.717386339238834d0*(r(i) - 1.0d-8)**2 + 0.125d0*tanh( &
      1.0000000100000002d0*r(i) - 1.0000000100000002d0) + 0.125d0*tanh( &
      1.0000000100000002d0*r(i) - 0.75000001000000005d0) + 0.125d0*tanh( &
      1.0000000100000002d0*r(i) - 0.50000001000000005d0) + 0.125d0*tanh( &
      1.0000000100000002d0*r(i) - 0.25000001000000011d0) + 0.125d0*tanh( &
      1.0000000100000002d0*r(i) - 1.0000000100000002d-8) + &
      0.737027635978981d0

      END DO
    END SUBROUTINE CalcPerturbationVariables
