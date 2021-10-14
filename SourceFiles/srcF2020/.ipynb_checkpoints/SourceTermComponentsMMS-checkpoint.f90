 
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
    S_1 =     S_1 = -i*(ak/(one + tanh((r - r2)*k(2))*k(1) + tanh((r - r3)*k(2))*k(1) + tanh &
      ((r - r_maxC C )*k(2))*k(1) + tanh((r2 - r_maxC C )*k(2))*k(1) + tanh((r3 &
      - r_maxC C )*k(2))*k(1)) - gam*cos((r - r_maxC C )*k(3))*k(3) - m*sqrt( &
      r*two*((1 - tanh((r - r2)*k(2))**2)*k(1)*k(2) + (1 - tanh((r - r3 &
      )*k(2))**2)*k(1)*k(2) + (1 - tanh((r - r_maxC C )*k(2))**2)*k(1)*k(2 &
      ))/((kappaCC - one)*(one + tanh((r - r2)*k(2))*k(1) + tanh((r - r3) &
      *k(2))*k(1) + tanh((r - r_maxC C )*k(2))*k(1) + tanh((r2 - r_maxC C )*k(2 &
      ))*k(1) + tanh((r3 - r_maxC C )*k(2))*k(1))))/r)*cos((r - r_maxC C )*k(4 &
      ))*k(4) - sin((r - r_maxC C )*k(7))*k(7)**2 - two*sqrt(r*two*((1 - &
      tanh((r - r2)*k(2))**2)*k(1)*k(2) + (1 - tanh((r - r3)*k(2))**2)* &
      k(1)*k(2) + (1 - tanh((r - r_maxC C )*k(2))**2)*k(1)*k(2))/((kappaCC - &
      one)*(one + tanh((r - r2)*k(2))*k(1) + tanh((r - r3)*k(2))*k(1) + &
      tanh((r - r_maxC C )*k(2))*k(1) + tanh((r2 - r_maxC C )*k(2))*k(1) + tanh &
      ((r3 - r_maxC C )*k(2))*k(1))))*cos((r - r_maxC C )*k(5))*k(5)/r + (r*two &
      *((1 - tanh((r - r2)*k(2))**2)*k(1)*k(2) + (1 - tanh((r - r3)*k(2 &
      ))**2)*k(1)*k(2) + (1 - tanh((r - r_maxC C )*k(2))**2)*k(1)*k(2))/(( &
      kappaCC - one)*(one + tanh((r - r2)*k(2))*k(1) + tanh((r - r3)*k(2 &
      ))*k(1) + tanh((r - r_maxC C )*k(2))*k(1) + tanh((r2 - r_maxC C )*k(2))*k &
      (1) + tanh((r3 - r_maxC C )*k(2))*k(1))))**((1.0d0/2.0d0)*two)*(kappaCC &
      - one)*cos((r - r_maxC C )*k(7))*k(7)/r

    S_2 =     S_2 = i*mC*cos((r - r_maxC C )*k(7))*k(7)/r - i*(ak/(one + tanh((r - r2)*k(2))*k(1 &
      ) + tanh((r - r3)*k(2))*k(1) + tanh((r - r_maxC C )*k(2))*k(1) + tanh &
      ((r2 - r_maxC C )*k(2))*k(1) + tanh((r3 - r_maxC C )*k(2))*k(1)) - gam* &
      cos((r - r_maxC C )*k(3))*k(3) - m*sqrt(r*two*((1 - tanh((r - r2)*k(2 &
      ))**2)*k(1)*k(2) + (1 - tanh((r - r3)*k(2))**2)*k(1)*k(2) + (1 - &
      tanh((r - r_maxC C )*k(2))**2)*k(1)*k(2))/((kappaCC - one)*(one + tanh &
      ((r - r2)*k(2))*k(1) + tanh((r - r3)*k(2))*k(1) + tanh((r - r_maxC C  &
      )*k(2))*k(1) + tanh((r2 - r_maxC C )*k(2))*k(1) + tanh((r3 - r_maxC C )*k &
      (2))*k(1))))/r)*cos((r - r_maxC C )*k(5))*k(5) + (sqrt(r*two*((1 - &
      tanh((r - r2)*k(2))**2)*k(1)*k(2) + (1 - tanh((r - r3)*k(2))**2)* &
      k(1)*k(2) + (1 - tanh((r - r_maxC C )*k(2))**2)*k(1)*k(2))/((kappaCC - &
      one)*(one + tanh((r - r2)*k(2))*k(1) + tanh((r - r3)*k(2))*k(1) + &
      tanh((r - r_maxC C )*k(2))*k(1) + tanh((r2 - r_maxC C )*k(2))*k(1) + tanh &
      ((r3 - r_maxC C )*k(2))*k(1))))/r + sqrt(r*two*((1 - tanh((r - r2)*k( &
      2))**2)*k(1)*k(2) + (1 - tanh((r - r3)*k(2))**2)*k(1)*k(2) + (1 - &
      tanh((r - r_maxC C )*k(2))**2)*k(1)*k(2))/((kappaCC - one)*(one + tanh &
      ((r - r2)*k(2))*k(1) + tanh((r - r3)*k(2))*k(1) + tanh((r - r_maxC C  &
      )*k(2))*k(1) + tanh((r2 - r_maxC C )*k(2))*k(1) + tanh((r3 - r_maxC C )*k &
      (2))*k(1))))*(kappaCC - one)*((1.0d0/2.0d0)*r*two*(-(1 - tanh((r - &
      r2)*k(2))**2)*k(1)*k(2) - (1 - tanh((r - r3)*k(2))**2)*k(1)*k(2) &
      - (1 - tanh((r - r_maxC C )*k(2))**2)*k(1)*k(2))*((1 - tanh((r - r2)* &
      k(2))**2)*k(1)*k(2) + (1 - tanh((r - r3)*k(2))**2)*k(1)*k(2) + (1 &
      - tanh((r - r_maxC C )*k(2))**2)*k(1)*k(2))/((kappaCC - one)*(one + &
      tanh((r - r2)*k(2))*k(1) + tanh((r - r3)*k(2))*k(1) + tanh((r - &
      r_maxC C )*k(2))*k(1) + tanh((r2 - r_maxC C )*k(2))*k(1) + tanh((r3 - &
      r_maxC C )*k(2))*k(1))**2) + (1.0d0/2.0d0)*r*two*(-2*(1 - tanh((r - &
      r2)*k(2))**2)*tanh((r - r2)*k(2))*k(1)*k(2)**2 - 2*(1 - tanh((r - &
      r3)*k(2))**2)*tanh((r - r3)*k(2))*k(1)*k(2)**2 - 2*(1 - tanh((r - &
      r_maxC C )*k(2))**2)*tanh((r - r_maxC C )*k(2))*k(1)*k(2)**2)/((kappaCC - &
      one)*(one + tanh((r - r2)*k(2))*k(1) + tanh((r - r3)*k(2))*k(1) + &
      tanh((r - r_maxC C )*k(2))*k(1) + tanh((r2 - r_maxC C )*k(2))*k(1) + tanh &
      ((r3 - r_maxC C )*k(2))*k(1))) + (1.0d0/2.0d0)*two*((1 - tanh((r - r2 &
      )*k(2))**2)*k(1)*k(2) + (1 - tanh((r - r3)*k(2))**2)*k(1)*k(2) + &
      (1 - tanh((r - r_maxC C )*k(2))**2)*k(1)*k(2))/((kappaCC - one)*(one + &
      tanh((r - r2)*k(2))*k(1) + tanh((r - r3)*k(2))*k(1) + tanh((r - &
      r_maxC C )*k(2))*k(1) + tanh((r2 - r_maxC C )*k(2))*k(1) + tanh((r3 - &
      r_maxC C )*k(2))*k(1))))*(one + tanh((r - r2)*k(2))*k(1) + tanh((r - &
      r3)*k(2))*k(1) + tanh((r - r_maxC C )*k(2))*k(1) + tanh((r2 - r_maxC C )* &
      k(2))*k(1) + tanh((r3 - r_maxC C )*k(2))*k(1))/(r*two*((1 - tanh((r - &
      r2)*k(2))**2)*k(1)*k(2) + (1 - tanh((r - r3)*k(2))**2)*k(1)*k(2) &
      + (1 - tanh((r - r_maxC C )*k(2))**2)*k(1)*k(2))) + (r*two*((1 - tanh &
      ((r - r2)*k(2))**2)*k(1)*k(2) + (1 - tanh((r - r3)*k(2))**2)*k(1) &
      *k(2) + (1 - tanh((r - r_maxC C )*k(2))**2)*k(1)*k(2))/((kappaCC - one) &
      *(one + tanh((r - r2)*k(2))*k(1) + tanh((r - r3)*k(2))*k(1) + &
      tanh((r - r_maxC C )*k(2))*k(1) + tanh((r2 - r_maxC C )*k(2))*k(1) + tanh &
      ((r3 - r_maxC C )*k(2))*k(1))))**((1.0d0/2.0d0)*three)*(kappaCC - one)/ &
      (r*two))*cos((r - r_maxC C )*k(4))*k(4)

    S_3 =     S_3 = gam*i*one*cos((r - r_maxC C )*k(7))*k(7) - i*(ak/(one + tanh((r - r2)*k(2 &
      ))*k(1) + tanh((r - r3)*k(2))*k(1) + tanh((r - r_maxC C )*k(2))*k(1) &
      + tanh((r2 - r_maxC C )*k(2))*k(1) + tanh((r3 - r_maxC C )*k(2))*k(1)) - &
      gam*cos((r - r_maxC C )*k(3))*k(3) - m*sqrt(r*two*((1 - tanh((r - &
      r2)*k(2))**2)*k(1)*k(2) + (1 - tanh((r - r3)*k(2))**2)*k(1)*k(2) &
      + (1 - tanh((r - r_maxC C )*k(2))**2)*k(1)*k(2))/((kappaCC - one)*(one &
      + tanh((r - r2)*k(2))*k(1) + tanh((r - r3)*k(2))*k(1) + tanh((r - &
      r_maxC C )*k(2))*k(1) + tanh((r2 - r_maxC C )*k(2))*k(1) + tanh((r3 - &
      r_maxC C )*k(2))*k(1))))/r)*cos((r - r_maxC C )*k(6))*k(6) + (-sin((r - &
      r_maxC C )*k(3))*k(3)**2 + (r*two*((1 - tanh((r - r2)*k(2))**2)*k(1)* &
      k(2) + (1 - tanh((r - r3)*k(2))**2)*k(1)*k(2) + (1 - tanh((r - &
      r_maxC C )*k(2))**2)*k(1)*k(2))/((kappaCC - one)*(one + tanh((r - r2)*k &
      (2))*k(1) + tanh((r - r3)*k(2))*k(1) + tanh((r - r_maxC C )*k(2))*k(1 &
      ) + tanh((r2 - r_maxC C )*k(2))*k(1) + tanh((r3 - r_maxC C )*k(2))*k(1 &
      ))))**((1.0d0/2.0d0)*two)*(kappaCC - one)*cos((r - r_maxC C )*k(3))*k(3 &
      )/(r*two))*cos((r - r_maxC C )*k(4))*k(4)

    S_4 =     S_4 = gam*i*one*cos((r - r_maxC C )*k(6))*k(6) + i*mC*cos((r - r_maxC C )*k(5))*k(5)/ &
      r - i*(ak/(one + tanh((r - r2)*k(2))*k(1) + tanh((r - r3)*k(2))*k &
      (1) + tanh((r - r_maxC C )*k(2))*k(1) + tanh((r2 - r_maxC C )*k(2))*k(1) &
      + tanh((r3 - r_maxC C )*k(2))*k(1)) - gam*cos((r - r_maxC C )*k(3))*k(3 &
      ) - m*sqrt(r*two*((1 - tanh((r - r2)*k(2))**2)*k(1)*k(2) + (1 - &
      tanh((r - r3)*k(2))**2)*k(1)*k(2) + (1 - tanh((r - r_maxC C )*k(2))** &
      2)*k(1)*k(2))/((kappaCC - one)*(one + tanh((r - r2)*k(2))*k(1) + &
      tanh((r - r3)*k(2))*k(1) + tanh((r - r_maxC C )*k(2))*k(1) + tanh((r2 &
      - r_maxC C )*k(2))*k(1) + tanh((r3 - r_maxC C )*k(2))*k(1))))/r)*cos((r - &
      r_maxC C )*k(7))*k(7) + (one/r + (r*two*((1 - tanh((r - r2)*k(2))**2) &
      *k(1)*k(2) + (1 - tanh((r - r3)*k(2))**2)*k(1)*k(2) + (1 - tanh(( &
      r - r_maxC C )*k(2))**2)*k(1)*k(2))/((kappaCC - one)*(one + tanh((r - &
      r2)*k(2))*k(1) + tanh((r - r3)*k(2))*k(1) + tanh((r - r_maxC C )*k(2 &
      ))*k(1) + tanh((r2 - r_maxC C )*k(2))*k(1) + tanh((r3 - r_maxC C )*k(2))* &
      k(1))))**((1.0d0/2.0d0)*two)*(kappaCC + one)/(r*two))*cos((r - &
      r_maxC C )*k(4))*k(4) - sin((r - r_maxC C )*k(4))*k(4)**2


    END SUBROUTINE SourceCalc
