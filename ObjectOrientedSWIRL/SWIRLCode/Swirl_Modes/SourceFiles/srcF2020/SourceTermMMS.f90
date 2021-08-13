 
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
    COMPLEX(KIND=rDef), INTENT(OUT) :: S_1, S_2, S_3, S_4
    COMPLEX(KIND=rDef), DIMENSION(:), INTENT(OUT) :: k
    
    S_1 = i*(-1.0d0/2.0d0*ak*exp(-(r - r_max)*k(1)) - gam*cos((r - r_max)*k(2)) &
      + 1.4142135623731d0*m*sqrt(r*k(1)/(kappa - 1.0d0))/r)*cos((r - &
      r_max)*k(3)) + sin((r - r_max)*k(6))*k(6) + 2.82842712474619d0* &
      sqrt(r*k(1)/(kappa - 1.0d0))*cos((r - r_max)*k(4))/r - 1.0d0*(r*k &
      (1)/(kappa - 1.0d0))**1.0d0*(kappa - 1.0d0)*cos((r - r_max)*k(6)) &
      /r
    S_2 = i*m*cos((r - r_max)*k(6))/r + i*(-1.0d0/2.0d0*ak*exp(-(r - r_max)*k(1)) &
      - gam*cos((r - r_max)*k(2)) + 1.4142135623731d0*m*sqrt(r*k(1)/( &
      kappa - 1.0d0))/r)*cos((r - r_max)*k(4)) + (-2.82842712474619d0*r &
      *(r*k(1)/(kappa - 1.0d0))**1.5d0*(0.5d0*kappa - 0.5d0) + &
      0.707106781186548d0*sqrt(r*k(1)/(kappa - 1.0d0))/r)*cos((r - &
      r_max)*k(3))
    S_3 = gam*i*cos((r - r_max)*k(6)) + i*(-1.0d0/2.0d0*ak*exp(-(r - r_max)*k(1 &
      )) - gam*cos((r - r_max)*k(2)) + 1.4142135623731d0*m*sqrt(r*k(1 &
      )/(kappa - 1.0d0))/r)*cos((r - r_max)*k(5)) + (-sin((r - r_max)*k &
      (2))*k(2) - 1.0d0*(r*k(1)/(kappa - 1.0d0))**1.0d0*(kappa - 1.0d0) &
      *cos((r - r_max)*k(2))/r)*cos((r - r_max)*k(3))
    S_4 = gam*i*cos((r - r_max)*k(5)) + i*m*cos((r - r_max)*k(4))/r + i*(-1.0d0/ &
      2.0d0*ak*exp(-(r - r_max)*k(1)) - gam*cos((r - r_max)*k(2)) + &
      1.4142135623731d0*m*sqrt(r*k(1)/(kappa - 1.0d0))/r)*cos((r - &
      r_max)*k(6)) + (1.0d0*(r*k(1)/(kappa - 1.0d0))**1.0d0*(kappa - &
      1.0d0)/r + 1.0d0/r)*cos((r - r_max)*k(3)) - sin((r - r_max)*k(3)) &
      *k(3)

    END SUBROUTINE SourceCalc
