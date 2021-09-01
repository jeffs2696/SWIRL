 
! gam - axial wavenumber 
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
    r_max, &
    S_1  , &
    S_2  , &
    S_3  , &
    S_4)
    
    INTEGER, INTENT(IN) :: m
    REAL(KIND=rDef)   , INTENT(IN) :: kappa,r,r_max 
    COMPLEX(KIND=rDef), INTENT(IN) :: i, gam, ak           
    COMPLEX(KIND=rDef), INTENT(INOUT) :: S_1, S_2, S_3, S_4
    COMPLEX(KIND=rDef), DIMENSION(:), INTENT(IN) :: k
    
    ! Local variables
    COMPLEX(KIND=rDef) :: mC, kappaC, rC, r_maxC
    
    COMPLEX(KIND=rDef) :: one,two,three
    
    one = CMPLX(1.0,KIND=rDef)    
    two = CMPLX(2.0,KIND=rDef)    
    three = CMPLX(3.0,KIND=rDef)
    
    mC = CMPLX(m,KIND=rDef)
    kappaC = CMPLX(kappa,KIND=rDef)
    rC = CMPLX(r,KIND=rDef)
    r_maxC = CMPLX(r_max,KIND=rDef)
    S_1 = i*(-ak/(one - tanh((one - r_maxC )*k(2))*k(1) + tanh((rC - r_maxC )*k(2))*k(1 &
      )) - gam*cos((rC - r_maxC )*k(2)) + mC*sqrt(rC*two*k(1)*k(2)/((kappaC &
      - one)*(one - tanh((one - r_maxC )*k(2))*k(1) + tanh((rC - r_maxC )*k( &
      2))*k(1))*cosh((rC - r_maxC )*k(2))**2))/rC)*cos((rC - r_maxC )*k(3)) + &
      sin((rC - r_maxC )*k(6))*k(6) + two*sqrt(rC*two*k(1)*k(2)/((kappaC - &
      one)*(one - tanh((one - r_maxC )*k(2))*k(1) + tanh((rC - r_maxC )*k(2 &
      ))*k(1))*cosh((rC - r_maxC )*k(2))**2))*cos((rC - r_maxC )*k(4))/rC - (rC &
      *two*k(1)*k(2)/((kappaC - one)*(one - tanh((one - r_maxC )*k(2))*k(1 &
      ) + tanh((rC - r_maxC )*k(2))*k(1))*cosh((rC - r_maxC )*k(2))**2))**(( &
      CMPLX(1.0_rDef,KIND=rDef)/CMPLX(2.0_rDef,KIND=rDef))*two)*(kappaC - one)*cos((rC - r_maxC )*k(6))/(rC*two)
    S_2 = i*mC*cos((rC - r_maxC )*k(6))/rC + i*(-ak/(one - tanh((one - r_maxC )*k(2))*k(1 &
      ) + tanh((rC - r_maxC )*k(2))*k(1)) - gam*cos((rC - r_maxC )*k(2)) + &
      mC*sqrt(rC*two*k(1)*k(2)/((kappaC - one)*(one - tanh((one - r_maxC )*k &
      (2))*k(1) + tanh((rC - r_maxC )*k(2))*k(1))*cosh((rC - r_maxC )*k(2))** &
      2))/rC)*cos((rC - r_maxC )*k(4)) + (sqrt(rC*two*k(1)*k(2)/((kappaC - &
      one)*(one - tanh((one - r_maxC )*k(2))*k(1) + tanh((rC - r_maxC )*k(2 &
      ))*k(1))*cosh((rC - r_maxC )*k(2))**2))/rC - sqrt(rC*two*k(1)*k(2)/(( &
      kappaC - one)*(one - tanh((one - r_maxC )*k(2))*k(1) + tanh((rC - &
      r_maxC )*k(2))*k(1))*cosh((rC - r_maxC )*k(2))**2))*(kappaC - one)*(one &
      - tanh((one - r_maxC )*k(2))*k(1) + tanh((rC - r_maxC )*k(2))*k(1))*( &
      -CMPLX(1.0_rDef,KIND=rDef)/CMPLX(2.0_rDef,KIND=rDef)*rC*two*(1 - tanh((rC - r_maxC )*k(2))**2)*k(1)**2*k(2)** &
      2/((kappaC - one)*(one - tanh((one - r_maxC )*k(2))*k(1) + tanh((rC - &
      r_maxC )*k(2))*k(1))**2*cosh((rC - r_maxC )*k(2))**2) - rC*two*sinh((rC &
      - r_maxC )*k(2))*k(1)*k(2)**2/((kappaC - one)*(one - tanh((one - &
      r_maxC )*k(2))*k(1) + tanh((rC - r_maxC )*k(2))*k(1))*cosh((rC - r_maxC ) &
      *k(2))**3) + (CMPLX(1.0_rDef,KIND=rDef)/CMPLX(2.0_rDef,KIND=rDef))*two*k(1)*k(2)/((kappaC - one)*(one - &
      tanh((one - r_maxC )*k(2))*k(1) + tanh((rC - r_maxC )*k(2))*k(1))*cosh &
      ((rC - r_maxC )*k(2))**2))*cosh((rC - r_maxC )*k(2))**2/(rC*two*k(1)*k(2 &
      )) - (rC*two*k(1)*k(2)/((kappaC - one)*(one - tanh((one - r_maxC )*k( &
      2))*k(1) + tanh((rC - r_maxC )*k(2))*k(1))*cosh((rC - r_maxC )*k(2))**2 &
      ))**((CMPLX(1.0_rDef,KIND=rDef)/CMPLX(2.0_rDef,KIND=rDef))*three)*(kappaC - one)/(rC*two))*cos((rC - r_maxC )* &
      k(3))
    S_3 = gam*i*cos((rC - r_maxC )*k(6)) + i*(-ak/(one - tanh((one - r_maxC )*k(2))*k &
      (1) + tanh((rC - r_maxC )*k(2))*k(1)) - gam*cos((rC - r_maxC )*k(2)) &
      + mC*sqrt(rC*two*k(1)*k(2)/((kappaC - one)*(one - tanh((one - r_maxC ) &
      *k(2))*k(1) + tanh((rC - r_maxC )*k(2))*k(1))*cosh((rC - r_maxC )*k(2)) &
      **2))/rC)*cos((rC - r_maxC )*k(5)) + (-sin((rC - r_maxC )*k(2))*k(2) - ( &
      rC*two*k(1)*k(2)/((kappaC - one)*(one - tanh((one - r_maxC )*k(2))*k( &
      1) + tanh((rC - r_maxC )*k(2))*k(1))*cosh((rC - r_maxC )*k(2))**2))**(( &
      CMPLX(1.0_rDef,KIND=rDef)/CMPLX(2.0_rDef,KIND=rDef))*two)*(kappaC - one)*cos((rC - r_maxC )*k(2))/(rC*two))* &
      cos((rC - r_maxC )*k(3))
    S_4 = gam*i*cos((rC - r_maxC )*k(5)) + i*mC*cos((rC - r_maxC )*k(4))/rC + i*(-ak/( &
      one - tanh((one - r_maxC )*k(2))*k(1) + tanh((rC - r_maxC )*k(2))*k(1 &
      )) - gam*cos((rC - r_maxC )*k(2)) + mC*sqrt(rC*two*k(1)*k(2)/((kappaC &
      - one)*(one - tanh((one - r_maxC )*k(2))*k(1) + tanh((rC - r_maxC )*k( &
      2))*k(1))*cosh((rC - r_maxC )*k(2))**2))/rC)*cos((rC - r_maxC )*k(6)) + &
      (one/rC + (rC*two*k(1)*k(2)/((kappaC - one)*(one - tanh((one - r_maxC  &
      )*k(2))*k(1) + tanh((rC - r_maxC )*k(2))*k(1))*cosh((rC - r_maxC )*k(2 &
      ))**2))**((CMPLX(1.0_rDef,KIND=rDef)/CMPLX(2.0_rDef,KIND=rDef))*two)*(kappaC - one)/(rC*two))*cos((rC - &
      r_maxC )*k(3)) - sin((rC - r_maxC )*k(3))*k(3)

    END SUBROUTINE SourceCalc
