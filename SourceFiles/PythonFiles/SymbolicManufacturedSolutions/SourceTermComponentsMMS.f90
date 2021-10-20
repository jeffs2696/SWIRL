 
! gam - axial wavenumber t
! ak  - reduced frequency
! kappa - ratio of specific heats
! i - imaginary number
 
    SUBROUTINE SourceCalcComponents(& 
    gam  , &
    i    , &
    ak   , &
    k    , &
    kappa, &
    m    , & 
    r    , &
    r2   , &
    r3   , &
    r_max, &
    S_1  , &
    S_2  , &
    S_3  , &
    S_4  , &
    S_A11, &
    S_A12, &
    S_A13, &
    S_A14, &
    S_A21, &
    S_A22, &
    S_A23, &
    S_A24, &
    S_A31, &
    S_A32, &
    S_A33, &
    S_A34, &
    S_A41, &
    S_A42, &
    S_A43, &
    S_A44, &
    S_B11, &
    S_B12, &
    S_B13, &
    S_B14, &
    S_B21, &
    S_B22, &
    S_B23, &
    S_B24, &
    S_B31, &
    S_B32, &
    S_B33, &
    S_B34, &
    S_B41, &
    S_B42, &
    S_B43, &
    S_B44 )
    
    INTEGER, INTENT(IN) :: m
    REAL(KIND=rDef)   , INTENT(IN) :: kappa,r,r2,r3,r_max
    !REAL(KIND=rDef)   , DIMENSION(:), INTENT(IN) :: r_loc
    COMPLEX(KIND=rDef), INTENT(IN) :: i, gam, ak           
    COMPLEX(KIND=rDef), INTENT(INOUT) :: &
    S_1,S_2,S_3,S_4, &
    S_A11, &
    S_A12, &
    S_A13, &
    S_A14, &
    S_A21, &
    S_A22, &
    S_A23, &
    S_A24, &
    S_A31, &
    S_A32, &
    S_A33, &
    S_A34, &
    S_A41, &
    S_A42, &
    S_A43, &
    S_A44, &
    S_B11, &
    S_B12, &
    S_B13, &
    S_B14, &
    S_B21, &
    S_B22, &
    S_B23, &
    S_B24, &
    S_B31, &
    S_B32, &
    S_B33, &
    S_B34, &
    S_B41, &
    S_B42, &
    S_B43, &
    S_B44
    
    COMPLEX(KIND=rDef), DIMENSION(:), INTENT(IN) :: k
    
    ! Local variables
    COMPLEX(KIND=rDef) :: mC, kappaC, rC, r_maxC, r2C, r3C
    
    COMPLEX(KIND=rDef) :: one,two,three

    one = CMPLX(1.0,KIND=rDef)    
    two = CMPLX(2.0,KIND=rDef)    
    three = CMPLX(3.0,KIND=rDef)

    mC = CMPLX(m,KIND=rDef)
    kappaC = CMPLX(kappa,KIND=rDef)
    rC = CMPLX(r,KIND=rDef)
    r2C = CMPLX(r2,KIND=rDef)
    r3C = CMPLX(r3,KIND=rDef)
    r_maxC = CMPLX(r_max,KIND=rDef)
S_A11=-i*(ak/(0.0833333333333333d0*tanh(100.0d0*r) + 0.0833333333333333d0*tanh &
      (100.0d0*r - 100.0d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      85.714285714285722d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      71.428571428571431d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      57.142857142857139d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      42.857142857142861d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      28.57142857142858d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      14.28571428571429d0) + 0.416666666666732d0) - m*sqrt(r*two*( &
      -8.33333333333333d0*tanh(100.0d0*r)**2 - 8.33333333333333d0*tanh( &
      100.0d0*r - 100.0d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      85.714285714285722d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      71.428571428571431d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      57.142857142857139d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      42.857142857142861d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      28.57142857142858d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      14.28571428571429d0)**2 + 66.6666666666666d0)/((kappaC - one)*( &
      0.0833333333333333d0*tanh(100.0d0*r) + 0.0833333333333333d0*tanh( &
      100.0d0*r - 100.0d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      85.714285714285722d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      71.428571428571431d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      57.142857142857139d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      42.857142857142861d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      28.57142857142858d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      14.28571428571429d0) + 0.416666666666732d0)))/r)*(cos((r - r_maxC) &
      *k(4)) - 1)
S_A12=-two*sqrt(r*two*(-8.33333333333333d0*tanh(100.0d0*r)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 100.0d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 85.714285714285722d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 71.428571428571431d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 57.142857142857139d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 42.857142857142861d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 28.57142857142858d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 14.28571428571429d0)**2 + &
      66.6666666666666d0)/((kappaC - one)*(0.0833333333333333d0*tanh( &
      100.0d0*r) + 0.0833333333333333d0*tanh(100.0d0*r - 100.0d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 85.714285714285722d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 71.428571428571431d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 57.142857142857139d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 42.857142857142861d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 28.57142857142858d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 14.28571428571429d0) + &
      0.416666666666732d0)))*sin((r - r_maxC)*k(5))/r
S_A13=0
S_A14=(-sin((r - r_maxC)*k(7))*k(7)/(cos((r - r_maxC)*k(7)) - 1) + (r*two*( &
      -8.33333333333333d0*tanh(100.0d0*r)**2 - 8.33333333333333d0*tanh( &
      100.0d0*r - 100.0d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      85.714285714285722d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      71.428571428571431d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      57.142857142857139d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      42.857142857142861d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      28.57142857142858d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      14.28571428571429d0)**2 + 66.6666666666666d0)/((kappaC - one)*( &
      0.0833333333333333d0*tanh(100.0d0*r) + 0.0833333333333333d0*tanh( &
      100.0d0*r - 100.0d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      85.714285714285722d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      71.428571428571431d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      57.142857142857139d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      42.857142857142861d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      28.57142857142858d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      14.28571428571429d0) + 0.416666666666732d0)))**((1.0d0/2.0d0)*two &
      )*(kappaC - one)/r)*(cos((r - r_maxC)*k(7)) - 1)
S_A21=(cos((r - r_maxC)*k(4)) - 1)*(sqrt(r*two*(-8.33333333333333d0*tanh( &
      100.0d0*r)**2 - 8.33333333333333d0*tanh(100.0d0*r - 100.0d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 85.714285714285722d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 71.428571428571431d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 57.142857142857139d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 42.857142857142861d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 28.57142857142858d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 14.28571428571429d0)**2 + &
      66.6666666666666d0)/((kappaC - one)*(0.0833333333333333d0*tanh( &
      100.0d0*r) + 0.0833333333333333d0*tanh(100.0d0*r - 100.0d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 85.714285714285722d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 71.428571428571431d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 57.142857142857139d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 42.857142857142861d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 28.57142857142858d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 14.28571428571429d0) + &
      0.416666666666732d0)))/r + sqrt(r*two*(-8.33333333333333d0*tanh( &
      100.0d0*r)**2 - 8.33333333333333d0*tanh(100.0d0*r - 100.0d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 85.714285714285722d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 71.428571428571431d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 57.142857142857139d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 42.857142857142861d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 28.57142857142858d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 14.28571428571429d0)**2 + &
      66.6666666666666d0)/((kappaC - one)*(0.0833333333333333d0*tanh( &
      100.0d0*r) + 0.0833333333333333d0*tanh(100.0d0*r - 100.0d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 85.714285714285722d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 71.428571428571431d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 57.142857142857139d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 42.857142857142861d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 28.57142857142858d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 14.28571428571429d0) + &
      0.416666666666732d0)))*(kappaC - one)*((1.0d0/2.0d0)*r*two*( &
      -8.33333333333333d0*(200 - 200*tanh(100.0d0*r)**2)*tanh(100.0d0*r &
      ) - 8.33333333333333d0*(200 - 200*tanh(100.0d0*r - 100.0d0)**2)* &
      tanh(100.0d0*r - 100.0d0) - 8.33333333333333d0*(200 - 200*tanh( &
      100.0d0*r - 85.714285714285722d0)**2)*tanh(100.0d0*r - &
      85.714285714285722d0) - 8.33333333333333d0*(200 - 200*tanh( &
      100.0d0*r - 71.428571428571431d0)**2)*tanh(100.0d0*r - &
      71.428571428571431d0) - 8.33333333333333d0*(200 - 200*tanh( &
      100.0d0*r - 57.142857142857139d0)**2)*tanh(100.0d0*r - &
      57.142857142857139d0) - 8.33333333333333d0*(200 - 200*tanh( &
      100.0d0*r - 42.857142857142861d0)**2)*tanh(100.0d0*r - &
      42.857142857142861d0) - 8.33333333333333d0*(200 - 200*tanh( &
      100.0d0*r - 28.57142857142858d0)**2)*tanh(100.0d0*r - &
      28.57142857142858d0) - 8.33333333333333d0*(200 - 200*tanh(100.0d0 &
      *r - 14.28571428571429d0)**2)*tanh(100.0d0*r - &
      14.28571428571429d0))/((kappaC - one)*(0.0833333333333333d0*tanh( &
      100.0d0*r) + 0.0833333333333333d0*tanh(100.0d0*r - 100.0d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 85.714285714285722d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 71.428571428571431d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 57.142857142857139d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 42.857142857142861d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 28.57142857142858d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 14.28571428571429d0) + &
      0.416666666666732d0)) + 2.8799999999991d0*r*two*( &
      -8.33333333333333d0*tanh(100.0d0*r)**2 - 8.33333333333333d0*tanh( &
      100.0d0*r - 100.0d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      85.714285714285722d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      71.428571428571431d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      57.142857142857139d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      42.857142857142861d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      28.57142857142858d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      14.28571428571429d0)**2 + 66.6666666666666d0)*(8.33333333333333d0 &
      *tanh(100.0d0*r)**2 + 8.33333333333333d0*tanh(100.0d0*r - 100.0d0 &
      )**2 + 8.33333333333333d0*tanh(100.0d0*r - 85.714285714285722d0) &
      **2 + 8.33333333333333d0*tanh(100.0d0*r - 71.428571428571431d0)** &
      2 + 8.33333333333333d0*tanh(100.0d0*r - 57.142857142857139d0)**2 &
      + 8.33333333333333d0*tanh(100.0d0*r - 42.857142857142861d0)**2 + &
      8.33333333333333d0*tanh(100.0d0*r - 28.57142857142858d0)**2 + &
      8.33333333333333d0*tanh(100.0d0*r - 14.28571428571429d0)**2 - &
      66.6666666666666d0)/((kappaC - one)*(0.199999999999969d0*tanh( &
      100.0d0*r) + 0.199999999999969d0*tanh(100.0d0*r - 100.0d0) + &
      0.199999999999969d0*tanh(100.0d0*r - 85.714285714285722d0) + &
      0.199999999999969d0*tanh(100.0d0*r - 71.428571428571431d0) + &
      0.199999999999969d0*tanh(100.0d0*r - 57.142857142857139d0) + &
      0.199999999999969d0*tanh(100.0d0*r - 42.857142857142861d0) + &
      0.199999999999969d0*tanh(100.0d0*r - 28.57142857142858d0) + &
      0.199999999999969d0*tanh(100.0d0*r - 14.28571428571429d0) + 1)**2 &
      ) + (1.0d0/2.0d0)*two*(-8.33333333333333d0*tanh(100.0d0*r)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 100.0d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 85.714285714285722d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 71.428571428571431d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 57.142857142857139d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 42.857142857142861d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 28.57142857142858d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 14.28571428571429d0)**2 + &
      66.6666666666666d0)/((kappaC - one)*(0.0833333333333333d0*tanh( &
      100.0d0*r) + 0.0833333333333333d0*tanh(100.0d0*r - 100.0d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 85.714285714285722d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 71.428571428571431d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 57.142857142857139d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 42.857142857142861d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 28.57142857142858d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 14.28571428571429d0) + &
      0.416666666666732d0)))*(0.0833333333333333d0*tanh(100.0d0*r) + &
      0.0833333333333333d0*tanh(100.0d0*r - 100.0d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 85.714285714285722d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 71.428571428571431d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 57.142857142857139d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 42.857142857142861d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 28.57142857142858d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 14.28571428571429d0) + &
      0.416666666666732d0)/(r*two*(-8.33333333333333d0*tanh(100.0d0*r) &
      **2 - 8.33333333333333d0*tanh(100.0d0*r - 100.0d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 85.714285714285722d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 71.428571428571431d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 57.142857142857139d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 42.857142857142861d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 28.57142857142858d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 14.28571428571429d0)**2 + &
      66.6666666666666d0)) + (r*two*(-8.33333333333333d0*tanh(100.0d0*r &
      )**2 - 8.33333333333333d0*tanh(100.0d0*r - 100.0d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 85.714285714285722d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 71.428571428571431d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 57.142857142857139d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 42.857142857142861d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 28.57142857142858d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 14.28571428571429d0)**2 + &
      66.6666666666666d0)/((kappaC - one)*(0.0833333333333333d0*tanh( &
      100.0d0*r) + 0.0833333333333333d0*tanh(100.0d0*r - 100.0d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 85.714285714285722d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 71.428571428571431d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 57.142857142857139d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 42.857142857142861d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 28.57142857142858d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 14.28571428571429d0) + &
      0.416666666666732d0)))**((1.0d0/2.0d0)*three)*(kappaC - one)/(r* &
      two))
S_A22=-i*(ak/(0.0833333333333333d0*tanh(100.0d0*r) + 0.0833333333333333d0*tanh &
      (100.0d0*r - 100.0d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      85.714285714285722d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      71.428571428571431d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      57.142857142857139d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      42.857142857142861d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      28.57142857142858d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      14.28571428571429d0) + 0.416666666666732d0) - m*sqrt(r*two*( &
      -8.33333333333333d0*tanh(100.0d0*r)**2 - 8.33333333333333d0*tanh( &
      100.0d0*r - 100.0d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      85.714285714285722d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      71.428571428571431d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      57.142857142857139d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      42.857142857142861d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      28.57142857142858d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      14.28571428571429d0)**2 + 66.6666666666666d0)/((kappaC - one)*( &
      0.0833333333333333d0*tanh(100.0d0*r) + 0.0833333333333333d0*tanh( &
      100.0d0*r - 100.0d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      85.714285714285722d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      71.428571428571431d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      57.142857142857139d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      42.857142857142861d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      28.57142857142858d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      14.28571428571429d0) + 0.416666666666732d0)))/r)*sin((r - r_maxC)* &
      k(5))
S_A23=0
S_A24=i*mC*(cos((r - r_maxC)*k(7)) - 1)/r
S_A31=(cos((r - r_maxC)*k(3))*k(3) + (r*two*(-8.33333333333333d0*tanh(100.0d0*r &
      )**2 - 8.33333333333333d0*tanh(100.0d0*r - 100.0d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 85.714285714285722d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 71.428571428571431d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 57.142857142857139d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 42.857142857142861d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 28.57142857142858d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 14.28571428571429d0)**2 + &
      66.6666666666666d0)/((kappaC - one)*(0.0833333333333333d0*tanh( &
      100.0d0*r) + 0.0833333333333333d0*tanh(100.0d0*r - 100.0d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 85.714285714285722d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 71.428571428571431d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 57.142857142857139d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 42.857142857142861d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 28.57142857142858d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 14.28571428571429d0) + &
      0.416666666666732d0)))**((1.0d0/2.0d0)*two)*(kappaC - one)*sin((r &
      - r_maxC)*k(3))/(r*two))*(cos((r - r_maxC)*k(4)) - 1)
S_A32=0
S_A33=-i*(ak/(0.0833333333333333d0*tanh(100.0d0*r) + 0.0833333333333333d0*tanh &
      (100.0d0*r - 100.0d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      85.714285714285722d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      71.428571428571431d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      57.142857142857139d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      42.857142857142861d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      28.57142857142858d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      14.28571428571429d0) + 0.416666666666732d0) - m*sqrt(r*two*( &
      -8.33333333333333d0*tanh(100.0d0*r)**2 - 8.33333333333333d0*tanh( &
      100.0d0*r - 100.0d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      85.714285714285722d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      71.428571428571431d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      57.142857142857139d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      42.857142857142861d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      28.57142857142858d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      14.28571428571429d0)**2 + 66.6666666666666d0)/((kappaC - one)*( &
      0.0833333333333333d0*tanh(100.0d0*r) + 0.0833333333333333d0*tanh( &
      100.0d0*r - 100.0d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      85.714285714285722d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      71.428571428571431d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      57.142857142857139d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      42.857142857142861d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      28.57142857142858d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      14.28571428571429d0) + 0.416666666666732d0)))/r)*sin((r - r_maxC)* &
      k(6))
S_A34=0
S_A41=(cos((r - r_maxC)*k(4)) - 1)*(one/r - sin((r - r_maxC)*k(4))*k(4)/(cos((r &
      - r_maxC)*k(4)) - 1) + (r*two*(-8.33333333333333d0*tanh(100.0d0*r) &
      **2 - 8.33333333333333d0*tanh(100.0d0*r - 100.0d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 85.714285714285722d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 71.428571428571431d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 57.142857142857139d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 42.857142857142861d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 28.57142857142858d0)**2 - &
      8.33333333333333d0*tanh(100.0d0*r - 14.28571428571429d0)**2 + &
      66.6666666666666d0)/((kappaC - one)*(0.0833333333333333d0*tanh( &
      100.0d0*r) + 0.0833333333333333d0*tanh(100.0d0*r - 100.0d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 85.714285714285722d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 71.428571428571431d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 57.142857142857139d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 42.857142857142861d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 28.57142857142858d0) + &
      0.0833333333333333d0*tanh(100.0d0*r - 14.28571428571429d0) + &
      0.416666666666732d0)))**((1.0d0/2.0d0)*two)*(kappaC + one)/(r*two &
      ))
S_A42=i*mC*sin((r - r_maxC)*k(5))/r
S_A43=0
S_A44=-i*(ak/(0.0833333333333333d0*tanh(100.0d0*r) + 0.0833333333333333d0*tanh &
      (100.0d0*r - 100.0d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      85.714285714285722d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      71.428571428571431d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      57.142857142857139d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      42.857142857142861d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      28.57142857142858d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      14.28571428571429d0) + 0.416666666666732d0) - m*sqrt(r*two*( &
      -8.33333333333333d0*tanh(100.0d0*r)**2 - 8.33333333333333d0*tanh( &
      100.0d0*r - 100.0d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      85.714285714285722d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      71.428571428571431d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      57.142857142857139d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      42.857142857142861d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      28.57142857142858d0)**2 - 8.33333333333333d0*tanh(100.0d0*r - &
      14.28571428571429d0)**2 + 66.6666666666666d0)/((kappaC - one)*( &
      0.0833333333333333d0*tanh(100.0d0*r) + 0.0833333333333333d0*tanh( &
      100.0d0*r - 100.0d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      85.714285714285722d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      71.428571428571431d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      57.142857142857139d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      42.857142857142861d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      28.57142857142858d0) + 0.0833333333333333d0*tanh(100.0d0*r - &
      14.28571428571429d0) + 0.416666666666732d0)))/r)*(cos((r - r_maxC) &
      *k(7)) - 1)
S_B11=-gam*i*(cos((r - r_maxC)*k(4)) - 1)*sin((r - r_maxC)*k(3))
S_B12=0
S_B13=0
S_B14=0
S_B21=0
S_B22=-gam*i*sin((r - r_maxC)*k(3))*sin((r - r_maxC)*k(5))
S_B23=0
S_B24=0
S_B31=0
S_B32=0
S_B33=-gam*i*sin((r - r_maxC)*k(3))*sin((r - r_maxC)*k(6))
S_B34=-gam*i*one*(cos((r - r_maxC)*k(7)) - 1)
S_B41=0
S_B42=0
S_B43=-gam*i*one*sin((r - r_maxC)*k(6))
S_B44=-gam*i*(cos((r - r_maxC)*k(7)) - 1)*sin((r - r_maxC)*k(3))


    if (r.eq.1.0_rDef) then
        S_A14 = CMPLX(0.0_rDef,KIND=rDef)
        S_A41 = CMPLX(0.0_rDef,KIND=rDef)
    elseif (r.eq.0.0_rDef) then
        S_A14 = CMPLX(0.0_rDef,KIND=rDef)
        S_A41 = CMPLX(0.0_rDef,KIND=rDef)
    else
    endif

    S_1 = &
    S_A11 +&
    S_A12 +&
    S_A13 +&
    S_A14 -(&
    S_B11 + &
    S_B12 + &
    S_B13 + &
    S_B14)
    
    S_2 = &
    S_A21 +&
    S_A22 +&
    S_A23 +&
    S_A24 -(&
    S_B21 + &
    S_B22 + &
    S_B23 + &
    S_B24)
    
    S_3 = &
    S_A31 +&
    S_A32 +&
    S_A33 +&
    S_A34 -(&
    S_B31 + &
    S_B32 + &
    S_B33 + &
    S_B34)
    
    S_4 = &
    S_A41 +&
    S_A42 +&
    S_A43 +&
    S_A44 -(&
    S_B41 + &
    S_B42 + &
    S_B43 + &
    S_B44)
    END SUBROUTINE SourceCalcComponents
