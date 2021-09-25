 
! gam - axial wavenumber t
! ak  - reduced frequency
! kappa - ratio of specific heats
! i - imaginary number
 
    SUBROUTINE SourceCalc(& 
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
    S_4)
    
    INTEGER, INTENT(IN) :: m
    REAL(KIND=rDef)   , INTENT(IN) :: kappa,r,r2,r3,r_max
    !REAL(KIND=rDef)   , DIMENSION(:), INTENT(IN) :: r_loc
    COMPLEX(KIND=rDef), INTENT(IN) :: i, gam, ak           
    COMPLEX(KIND=rDef), INTENT(INOUT) :: S_1, S_2, S_3, S_4
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
    S_1 = -i*(ak/(one + tanh((rC - r2C)*k(2))*k(1) + tanh((rC - r3C)*k(2))*k(1) + tanh &
      ((rC - r_maxC )*k(2))*k(1) + tanh((r2C - r_maxC )*k(2))*k(1) + tanh((r3C &
      - r_maxC )*k(2))*k(1)) - gam*cos((rC - r_maxC )*k(3))*k(3) - mC*sqrt( &
      rC*two*((1 - tanh((rC - r2C)*k(2))**2)*k(1)*k(2) + (1 - tanh((rC - r3C &
      )*k(2))**2)*k(1)*k(2) + (1 - tanh((rC - r_maxC )*k(2))**2)*k(1)*k(2 &
      ))/((kappaC - one)*(one + tanh((rC - r2C)*k(2))*k(1) + tanh((rC - r3C) &
      *k(2))*k(1) + tanh((rC - r_maxC )*k(2))*k(1) + tanh((r2C - r_maxC )*k(2 &
      ))*k(1) + tanh((r3C - r_maxC )*k(2))*k(1))))/rC)*cos((rC - r_maxC )*k(4 &
      ))*k(4) - sin((rC - r_maxC )*k(7))*k(7)**2 - two*sqrt(rC*two*((1 - &
      tanh((rC - r2C)*k(2))**2)*k(1)*k(2) + (1 - tanh((rC - r3C)*k(2))**2)* &
      k(1)*k(2) + (1 - tanh((rC - r_maxC )*k(2))**2)*k(1)*k(2))/((kappaC - &
      one)*(one + tanh((rC - r2C)*k(2))*k(1) + tanh((rC - r3C)*k(2))*k(1) + &
      tanh((rC - r_maxC )*k(2))*k(1) + tanh((r2C - r_maxC )*k(2))*k(1) + tanh &
      ((r3C - r_maxC )*k(2))*k(1))))*cos((rC - r_maxC )*k(5))*k(5)/rC + (rC*two &
      *((1 - tanh((rC - r2C)*k(2))**2)*k(1)*k(2) + (1 - tanh((rC - r3C)*k(2 &
      ))**2)*k(1)*k(2) + (1 - tanh((rC - r_maxC )*k(2))**2)*k(1)*k(2))/(( &
      kappaC - one)*(one + tanh((rC - r2C)*k(2))*k(1) + tanh((rC - r3C)*k(2 &
      ))*k(1) + tanh((rC - r_maxC )*k(2))*k(1) + tanh((r2C - r_maxC )*k(2))*k &
      (1) + tanh((r3C - r_maxC )*k(2))*k(1))))**((CMPLX(1.0_rDef,KIND=rDef)/CMPLX(2.0_rDef,KIND=rDef))*two)*(kappaC &
      - one)*cos((rC - r_maxC )*k(7))*k(7)/rC
    S_2 = i*mC*cos((rC - r_maxC )*k(7))*k(7)/rC - i*(ak/(one + tanh((rC - r2C)*k(2))*k(1 &
      ) + tanh((rC - r3C)*k(2))*k(1) + tanh((rC - r_maxC )*k(2))*k(1) + tanh &
      ((r2C - r_maxC )*k(2))*k(1) + tanh((r3C - r_maxC )*k(2))*k(1)) - gam* &
      cos((rC - r_maxC )*k(3))*k(3) - mC*sqrt(rC*two*((1 - tanh((rC - r2C)*k(2 &
      ))**2)*k(1)*k(2) + (1 - tanh((rC - r3C)*k(2))**2)*k(1)*k(2) + (1 - &
      tanh((rC - r_maxC )*k(2))**2)*k(1)*k(2))/((kappaC - one)*(one + tanh &
      ((rC - r2C)*k(2))*k(1) + tanh((rC - r3C)*k(2))*k(1) + tanh((rC - r_maxC  &
      )*k(2))*k(1) + tanh((r2C - r_maxC )*k(2))*k(1) + tanh((r3C - r_maxC )*k &
      (2))*k(1))))/rC)*cos((rC - r_maxC )*k(5))*k(5) + (sqrt(rC*two*((1 - &
      tanh((rC - r2C)*k(2))**2)*k(1)*k(2) + (1 - tanh((rC - r3C)*k(2))**2)* &
      k(1)*k(2) + (1 - tanh((rC - r_maxC )*k(2))**2)*k(1)*k(2))/((kappaC - &
      one)*(one + tanh((rC - r2C)*k(2))*k(1) + tanh((rC - r3C)*k(2))*k(1) + &
      tanh((rC - r_maxC )*k(2))*k(1) + tanh((r2C - r_maxC )*k(2))*k(1) + tanh &
      ((r3C - r_maxC )*k(2))*k(1))))/rC + sqrt(rC*two*((1 - tanh((rC - r2C)*k( &
      2))**2)*k(1)*k(2) + (1 - tanh((rC - r3C)*k(2))**2)*k(1)*k(2) + (1 - &
      tanh((rC - r_maxC )*k(2))**2)*k(1)*k(2))/((kappaC - one)*(one + tanh &
      ((rC - r2C)*k(2))*k(1) + tanh((rC - r3C)*k(2))*k(1) + tanh((rC - r_maxC  &
      )*k(2))*k(1) + tanh((r2C - r_maxC )*k(2))*k(1) + tanh((r3C - r_maxC )*k &
      (2))*k(1))))*(kappaC - one)*((CMPLX(1.0_rDef,KIND=rDef)/CMPLX(2.0_rDef,KIND=rDef))*rC*two*(-(1 - tanh((rC - &
      r2C)*k(2))**2)*k(1)*k(2) - (1 - tanh((rC - r3C)*k(2))**2)*k(1)*k(2) &
      - (1 - tanh((rC - r_maxC )*k(2))**2)*k(1)*k(2))*((1 - tanh((rC - r2C)* &
      k(2))**2)*k(1)*k(2) + (1 - tanh((rC - r3C)*k(2))**2)*k(1)*k(2) + (1 &
      - tanh((rC - r_maxC )*k(2))**2)*k(1)*k(2))/((kappaC - one)*(one + &
      tanh((rC - r2C)*k(2))*k(1) + tanh((rC - r3C)*k(2))*k(1) + tanh((rC - &
      r_maxC )*k(2))*k(1) + tanh((r2C - r_maxC )*k(2))*k(1) + tanh((r3C - &
      r_maxC )*k(2))*k(1))**2) + (CMPLX(1.0_rDef,KIND=rDef)/CMPLX(2.0_rDef,KIND=rDef))*rC*two*(-2*(1 - tanh((rC - &
      r2C)*k(2))**2)*tanh((rC - r2C)*k(2))*k(1)*k(2)**2 - 2*(1 - tanh((rC - &
      r3C)*k(2))**2)*tanh((rC - r3C)*k(2))*k(1)*k(2)**2 - 2*(1 - tanh((rC - &
      r_maxC )*k(2))**2)*tanh((rC - r_maxC )*k(2))*k(1)*k(2)**2)/((kappaC - &
      one)*(one + tanh((rC - r2C)*k(2))*k(1) + tanh((rC - r3C)*k(2))*k(1) + &
      tanh((rC - r_maxC )*k(2))*k(1) + tanh((r2C - r_maxC )*k(2))*k(1) + tanh &
      ((r3C - r_maxC )*k(2))*k(1))) + (CMPLX(1.0_rDef,KIND=rDef)/CMPLX(2.0_rDef,KIND=rDef))*two*((1 - tanh((rC - r2C &
      )*k(2))**2)*k(1)*k(2) + (1 - tanh((rC - r3C)*k(2))**2)*k(1)*k(2) + &
      (1 - tanh((rC - r_maxC )*k(2))**2)*k(1)*k(2))/((kappaC - one)*(one + &
      tanh((rC - r2C)*k(2))*k(1) + tanh((rC - r3C)*k(2))*k(1) + tanh((rC - &
      r_maxC )*k(2))*k(1) + tanh((r2C - r_maxC )*k(2))*k(1) + tanh((r3C - &
      r_maxC )*k(2))*k(1))))*(one + tanh((rC - r2C)*k(2))*k(1) + tanh((rC - &
      r3C)*k(2))*k(1) + tanh((rC - r_maxC )*k(2))*k(1) + tanh((r2C - r_maxC )* &
      k(2))*k(1) + tanh((r3C - r_maxC )*k(2))*k(1))/(rC*two*((1 - tanh((rC - &
      r2C)*k(2))**2)*k(1)*k(2) + (1 - tanh((rC - r3C)*k(2))**2)*k(1)*k(2) &
      + (1 - tanh((rC - r_maxC )*k(2))**2)*k(1)*k(2))) + (rC*two*((1 - tanh &
      ((rC - r2C)*k(2))**2)*k(1)*k(2) + (1 - tanh((rC - r3C)*k(2))**2)*k(1) &
      *k(2) + (1 - tanh((rC - r_maxC )*k(2))**2)*k(1)*k(2))/((kappaC - one) &
      *(one + tanh((rC - r2C)*k(2))*k(1) + tanh((rC - r3C)*k(2))*k(1) + &
      tanh((rC - r_maxC )*k(2))*k(1) + tanh((r2C - r_maxC )*k(2))*k(1) + tanh &
      ((r3C - r_maxC )*k(2))*k(1))))**((CMPLX(1.0_rDef,KIND=rDef)/CMPLX(2.0_rDef,KIND=rDef))*three)*(kappaC - one)/ &
      (rC*two))*cos((rC - r_maxC )*k(4))*k(4)
    S_3 = gam*i*one*cos((rC - r_maxC )*k(7))*k(7) - i*(ak/(one + tanh((rC - r2C)*k(2 &
      ))*k(1) + tanh((rC - r3C)*k(2))*k(1) + tanh((rC - r_maxC )*k(2))*k(1) &
      + tanh((r2C - r_maxC )*k(2))*k(1) + tanh((r3C - r_maxC )*k(2))*k(1)) - &
      gam*cos((rC - r_maxC )*k(3))*k(3) - mC*sqrt(rC*two*((1 - tanh((rC - &
      r2C)*k(2))**2)*k(1)*k(2) + (1 - tanh((rC - r3C)*k(2))**2)*k(1)*k(2) &
      + (1 - tanh((rC - r_maxC )*k(2))**2)*k(1)*k(2))/((kappaC - one)*(one &
      + tanh((rC - r2C)*k(2))*k(1) + tanh((rC - r3C)*k(2))*k(1) + tanh((rC - &
      r_maxC )*k(2))*k(1) + tanh((r2C - r_maxC )*k(2))*k(1) + tanh((r3C - &
      r_maxC )*k(2))*k(1))))/rC)*cos((rC - r_maxC )*k(6))*k(6) + (-sin((rC - &
      r_maxC )*k(3))*k(3)**2 + (rC*two*((1 - tanh((rC - r2C)*k(2))**2)*k(1)* &
      k(2) + (1 - tanh((rC - r3C)*k(2))**2)*k(1)*k(2) + (1 - tanh((rC - &
      r_maxC )*k(2))**2)*k(1)*k(2))/((kappaC - one)*(one + tanh((rC - r2C)*k &
      (2))*k(1) + tanh((rC - r3C)*k(2))*k(1) + tanh((rC - r_maxC )*k(2))*k(1 &
      ) + tanh((r2C - r_maxC )*k(2))*k(1) + tanh((r3C - r_maxC )*k(2))*k(1 &
      ))))**((CMPLX(1.0_rDef,KIND=rDef)/CMPLX(2.0_rDef,KIND=rDef))*two)*(kappaC - one)*cos((rC - r_maxC )*k(3))*k(3 &
      )/(rC*two))*cos((rC - r_maxC )*k(4))*k(4)
    S_4 = gam*i*one*cos((rC - r_maxC )*k(6))*k(6) + i*mC*cos((rC - r_maxC )*k(5))*k(5)/ &
      rC - i*(ak/(one + tanh((rC - r2C)*k(2))*k(1) + tanh((rC - r3C)*k(2))*k &
      (1) + tanh((rC - r_maxC )*k(2))*k(1) + tanh((r2C - r_maxC )*k(2))*k(1) &
      + tanh((r3C - r_maxC )*k(2))*k(1)) - gam*cos((rC - r_maxC )*k(3))*k(3 &
      ) - mC*sqrt(rC*two*((1 - tanh((rC - r2C)*k(2))**2)*k(1)*k(2) + (1 - &
      tanh((rC - r3C)*k(2))**2)*k(1)*k(2) + (1 - tanh((rC - r_maxC )*k(2))** &
      2)*k(1)*k(2))/((kappaC - one)*(one + tanh((rC - r2C)*k(2))*k(1) + &
      tanh((rC - r3C)*k(2))*k(1) + tanh((rC - r_maxC )*k(2))*k(1) + tanh((r2C &
      - r_maxC )*k(2))*k(1) + tanh((r3C - r_maxC )*k(2))*k(1))))/rC)*cos((rC - &
      r_maxC )*k(7))*k(7) + (one/rC + (rC*two*((1 - tanh((rC - r2C)*k(2))**2) &
      *k(1)*k(2) + (1 - tanh((rC - r3C)*k(2))**2)*k(1)*k(2) + (1 - tanh(( &
      rC - r_maxC )*k(2))**2)*k(1)*k(2))/((kappaC - one)*(one + tanh((rC - &
      r2C)*k(2))*k(1) + tanh((rC - r3C)*k(2))*k(1) + tanh((rC - r_maxC )*k(2 &
      ))*k(1) + tanh((r2C - r_maxC )*k(2))*k(1) + tanh((r3C - r_maxC )*k(2))* &
      k(1))))**((CMPLX(1.0_rDef,KIND=rDef)/CMPLX(2.0_rDef,KIND=rDef))*two)*(kappaC + one)/(rC*two))*cos((rC - &
      r_maxC )*k(4))*k(4) - sin((rC - r_maxC )*k(4))*k(4)**2

    END SUBROUTINE SourceCalc
