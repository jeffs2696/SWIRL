 
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
    
    mC = CMPLX(m,KIND=rDef)
    kappaC = CMPLX(kappa,KIND=rDef)
    rC = CMPLX(r,KIND=rDef)
    r_maxC = CMPLX(r_max,KIND=rDef)
    S_1 = i*(-ak/(tanh((r - r_maxC )*k(2))*k(1) + 1.0d0) - gam*cos((r - r_maxC )*k(2 &
      )) + 1.4142135623731d0*mC*sqrt(r*(1 - tanh((r - r_maxC )*k(2))**2)* &
      1d0/(tanh((r - r_maxC )*k(2))*k(1) + 1.0d0)*k(1)*k(2)/(kappaC - &
      1.0d0))/r)*cos((r - r_maxC )*k(3)) + sin((r - r_maxC )*k(6))*k(6) + &
      2.82842712474619d0*sqrt(r*(1 - tanh((r - r_maxC )*k(2))**2)*1d0/( &
      tanh((r - r_maxC )*k(2))*k(1) + 1.0d0)*k(1)*k(2)/(kappaC - 1.0d0))* &
      cos((r - r_maxC )*k(4))/r - 1.0d0*(r*(1 - tanh((r - r_maxC )*k(2))**2 &
      )*1d0/(tanh((r - r_maxC )*k(2))*k(1) + 1.0d0)*k(1)*k(2)/(kappaC - &
      1.0d0))**1.0d0*(kappaC - 1.0d0)*cos((r - r_maxC )*k(6))/r
    S_2 = i*mC*cos((r - r_maxC )*k(6))/r + i*(-ak/(tanh((r - r_maxC )*k(2))*k(1) + &
      1.0d0) - gam*cos((r - r_maxC )*k(2)) + 1.4142135623731d0*mC*sqrt(r &
      *(1 - tanh((r - r_maxC )*k(2))**2)*1d0/(tanh((r - r_maxC )*k(2))*k(1 &
      ) + 1.0d0)*k(1)*k(2)/(kappaC - 1.0d0))/r)*cos((r - r_maxC )*k(4)) + &
      (-2.82842712474619d0*r*(r*(1 - tanh((r - r_maxC )*k(2))**2)*1d0/( &
      tanh((r - r_maxC )*k(2))*k(1) + 1.0d0)*k(1)*k(2)/(kappaC - 1.0d0))** &
      1.5d0*(0.5d0*kappaC - 0.5d0) + 1.4142135623731d0*sqrt(r*(1 - tanh &
      ((r - r_maxC )*k(2))**2)*1d0/(tanh((r - r_maxC )*k(2))*k(1) + 1.0d0)* &
      k(1)*k(2)/(kappaC - 1.0d0))/r - 1.4142135623731d0*sqrt(r*(1 - tanh &
      ((r - r_maxC )*k(2))**2)*1d0/(tanh((r - r_maxC )*k(2))*k(1) + 1.0d0)* &
      k(1)*k(2)/(kappaC - 1.0d0))*(kappaC - 1.0d0)*(tanh((r - r_maxC )*k(2 &
      ))*k(1) + 1.0d0)**1.0d0*(-0.5d0*r*(1 - tanh((r - r_maxC )*k(2))**2) &
      **2*(tanh((r - r_maxC )*k(2))*k(1) + 1.0d0)**(-2.0d0)*k(1)**2*k(2) &
      **2/(kappaC - 1.0d0) - r*(1 - tanh((r - r_maxC )*k(2))**2)*1d0/(tanh &
      ((r - r_maxC )*k(2))*k(1) + 1.0d0)*tanh((r - r_maxC )*k(2))*k(1)*k(2) &
      **2/(kappaC - 1.0d0) + (1.0d0/2.0d0)*(1 - tanh((r - r_maxC )*k(2))** &
      2)*1d0/(tanh((r - r_maxC )*k(2))*k(1) + 1.0d0)*k(1)*k(2)/(kappaC - &
      1.0d0))/(r*(1 - tanh((r - r_maxC )*k(2))**2)*k(1)*k(2)))*cos((r - &
      r_maxC )*k(3))
    S_3 = gam*i*cos((r - r_maxC )*k(6)) + i*(-ak/(tanh((r - r_maxC )*k(2))*k(1) + &
      1.0d0) - gam*cos((r - r_maxC )*k(2)) + 1.4142135623731d0*mC*sqrt(r &
      *(1 - tanh((r - r_maxC )*k(2))**2)*1d0/(tanh((r - r_maxC )*k(2))*k(1 &
      ) + 1.0d0)*k(1)*k(2)/(kappaC - 1.0d0))/r)*cos((r - r_maxC )*k(5)) + &
      (-sin((r - r_maxC )*k(2))*k(2) - 1.0d0*(r*(1 - tanh((r - r_maxC )*k(2 &
      ))**2)*1d0/(tanh((r - r_maxC )*k(2))*k(1) + 1.0d0)*k(1)*k(2)/(kappaC &
      - 1.0d0))**1.0d0*(kappaC - 1.0d0)*cos((r - r_maxC )*k(2))/r)*cos((r &
      - r_maxC )*k(3))
    S_4 = gam*i*cos((r - r_maxC )*k(5)) + i*mC*cos((r - r_maxC )*k(4))/r + i*(-ak/( &
      tanh((r - r_maxC )*k(2))*k(1) + 1.0d0) - gam*cos((r - r_maxC )*k(2 &
      )) + 1.4142135623731d0*mC*sqrt(r*(1 - tanh((r - r_maxC )*k(2))**2)* &
      1d0/(tanh((r - r_maxC )*k(2))*k(1) + 1.0d0)*k(1)*k(2)/(kappaC - &
      1.0d0))/r)*cos((r - r_maxC )*k(6)) + (1.0d0*(r*(1 - tanh((r - r_maxC  &
      )*k(2))**2)*1d0/(tanh((r - r_maxC )*k(2))*k(1) + 1.0d0)*k(1)*k(2)/( &
      kappaC - 1.0d0))**1.0d0*(kappaC - 1.0d0)/r + 1.0d0/r)*cos((r - &
      r_maxC )*k(3)) - sin((r - r_maxC )*k(3))*k(3)

    END SUBROUTINE SourceCalc
