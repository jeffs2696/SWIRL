 
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
S_A11=-i*(ak/(one + tanh((r - r2)*k(2))*k(1) + tanh((r - r3)*k(2))*k(1) + tanh &
      ((r - r_maxC)*k(2))*k(1) + tanh((r2 - r_maxC)*k(2))*k(1) + tanh((r3 &
      - r_maxC)*k(2))*k(1)) - m*sqrt(r*two*((1 - tanh((r - r2)*k(2))**2) &
      *k(1)*k(2) + (1 - tanh((r - r3)*k(2))**2)*k(1)*k(2) + (1 - tanh(( &
      r - r_maxC)*k(2))**2)*k(1)*k(2))/((kappaC - one)*(one + tanh((r - &
      r2)*k(2))*k(1) + tanh((r - r3)*k(2))*k(1) + tanh((r - r_maxC)*k(2 &
      ))*k(1) + tanh((r2 - r_maxC)*k(2))*k(1) + tanh((r3 - r_maxC)*k(2))* &
      k(1))))/r)*(cos((r - r_maxC)*k(4)) - 1)
S_A12=-two*sqrt(r*two*((1 - tanh((r - r2)*k(2))**2)*k(1)*k(2) + (1 - tanh((r - &
      r3)*k(2))**2)*k(1)*k(2) + (1 - tanh((r - r_maxC)*k(2))**2)*k(1)*k( &
      2))/((kappaC - one)*(one + tanh((r - r2)*k(2))*k(1) + tanh((r - r3 &
      )*k(2))*k(1) + tanh((r - r_maxC)*k(2))*k(1) + tanh((r2 - r_maxC)*k( &
      2))*k(1) + tanh((r3 - r_maxC)*k(2))*k(1))))*sin((r - r_maxC)*k(5))/ &
    r
S_A13=0
if (r.eq.1.0_rDef .or. r.eq.0.2_rDef) then
    S_A14 = 0.0_rDef
    S_A13 = 0.0_rDef
    S_A12 = 0.0_rDef
    S_A11 = 0.0_rDef
else
    S_A14=(-sin((r - r_maxC)*k(7))*k(7)/(cos((r - r_maxC)*k(7)) - 1) + (r*two*((1 - &
        tanh((r - r2)*k(2))**2)*k(1)*k(2) + (1 - tanh((r - r3)*k(2))**2)* &
        k(1)*k(2) + (1 - tanh((r - r_maxC)*k(2))**2)*k(1)*k(2))/((kappaC - &
        one)*(one + tanh((r - r2)*k(2))*k(1) + tanh((r - r3)*k(2))*k(1) + &
        tanh((r - r_maxC)*k(2))*k(1) + tanh((r2 - r_maxC)*k(2))*k(1) + tanh &
        ((r3 - r_maxC)*k(2))*k(1))))**((1.0d0/2.0d0)*two)*(kappaC - one)/r) &
        *(cos((r - r_maxC)*k(7)) - 1)
endif

S_A21=(cos((r - r_maxC)*k(4)) - 1)*(sqrt(r*two*((1 - tanh((r - r2)*k(2))**2)*k( &
      1)*k(2) + (1 - tanh((r - r3)*k(2))**2)*k(1)*k(2) + (1 - tanh((r - &
      r_maxC)*k(2))**2)*k(1)*k(2))/((kappaC - one)*(one + tanh((r - r2)*k &
      (2))*k(1) + tanh((r - r3)*k(2))*k(1) + tanh((r - r_maxC)*k(2))*k(1 &
      ) + tanh((r2 - r_maxC)*k(2))*k(1) + tanh((r3 - r_maxC)*k(2))*k(1 &
      ))))/r + sqrt(r*two*((1 - tanh((r - r2)*k(2))**2)*k(1)*k(2) + (1 &
      - tanh((r - r3)*k(2))**2)*k(1)*k(2) + (1 - tanh((r - r_maxC)*k(2)) &
      **2)*k(1)*k(2))/((kappaC - one)*(one + tanh((r - r2)*k(2))*k(1) + &
      tanh((r - r3)*k(2))*k(1) + tanh((r - r_maxC)*k(2))*k(1) + tanh((r2 &
      - r_maxC)*k(2))*k(1) + tanh((r3 - r_maxC)*k(2))*k(1))))*(kappaC - &
      one)*((1.0d0/2.0d0)*r*two*(-(1 - tanh((r - r2)*k(2))**2)*k(1)*k(2 &
      ) - (1 - tanh((r - r3)*k(2))**2)*k(1)*k(2) - (1 - tanh((r - r_maxC &
      )*k(2))**2)*k(1)*k(2))*((1 - tanh((r - r2)*k(2))**2)*k(1)*k(2) + &
      (1 - tanh((r - r3)*k(2))**2)*k(1)*k(2) + (1 - tanh((r - r_maxC)*k( &
      2))**2)*k(1)*k(2))/((kappaC - one)*(one + tanh((r - r2)*k(2))*k(1 &
      ) + tanh((r - r3)*k(2))*k(1) + tanh((r - r_maxC)*k(2))*k(1) + tanh &
      ((r2 - r_maxC)*k(2))*k(1) + tanh((r3 - r_maxC)*k(2))*k(1))**2) + ( &
      1.0d0/2.0d0)*r*two*(-2*(1 - tanh((r - r2)*k(2))**2)*tanh((r - r2) &
      *k(2))*k(1)*k(2)**2 - 2*(1 - tanh((r - r3)*k(2))**2)*tanh((r - r3 &
      )*k(2))*k(1)*k(2)**2 - 2*(1 - tanh((r - r_maxC)*k(2))**2)*tanh((r &
      - r_maxC)*k(2))*k(1)*k(2)**2)/((kappaC - one)*(one + tanh((r - r2)* &
      k(2))*k(1) + tanh((r - r3)*k(2))*k(1) + tanh((r - r_maxC)*k(2))*k( &
      1) + tanh((r2 - r_maxC)*k(2))*k(1) + tanh((r3 - r_maxC)*k(2))*k(1 &
      ))) + (1.0d0/2.0d0)*two*((1 - tanh((r - r2)*k(2))**2)*k(1)*k(2) + &
      (1 - tanh((r - r3)*k(2))**2)*k(1)*k(2) + (1 - tanh((r - r_maxC)*k( &
      2))**2)*k(1)*k(2))/((kappaC - one)*(one + tanh((r - r2)*k(2))*k(1 &
      ) + tanh((r - r3)*k(2))*k(1) + tanh((r - r_maxC)*k(2))*k(1) + tanh &
      ((r2 - r_maxC)*k(2))*k(1) + tanh((r3 - r_maxC)*k(2))*k(1))))*(one + &
      tanh((r - r2)*k(2))*k(1) + tanh((r - r3)*k(2))*k(1) + tanh((r - &
      r_maxC)*k(2))*k(1) + tanh((r2 - r_maxC)*k(2))*k(1) + tanh((r3 - &
      r_maxC)*k(2))*k(1))/(r*two*((1 - tanh((r - r2)*k(2))**2)*k(1)*k(2 &
      ) + (1 - tanh((r - r3)*k(2))**2)*k(1)*k(2) + (1 - tanh((r - r_maxC &
      )*k(2))**2)*k(1)*k(2))) + (r*two*((1 - tanh((r - r2)*k(2))**2)*k( &
      1)*k(2) + (1 - tanh((r - r3)*k(2))**2)*k(1)*k(2) + (1 - tanh((r - &
      r_maxC)*k(2))**2)*k(1)*k(2))/((kappaC - one)*(one + tanh((r - r2)*k &
      (2))*k(1) + tanh((r - r3)*k(2))*k(1) + tanh((r - r_maxC)*k(2))*k(1 &
      ) + tanh((r2 - r_maxC)*k(2))*k(1) + tanh((r3 - r_maxC)*k(2))*k(1 &
      ))))**((1.0d0/2.0d0)*three)*(kappaC - one)/(r*two))
S_A22=-i*(ak/(one + tanh((r - r2)*k(2))*k(1) + tanh((r - r3)*k(2))*k(1) + tanh &
      ((r - r_maxC)*k(2))*k(1) + tanh((r2 - r_maxC)*k(2))*k(1) + tanh((r3 &
      - r_maxC)*k(2))*k(1)) - m*sqrt(r*two*((1 - tanh((r - r2)*k(2))**2) &
      *k(1)*k(2) + (1 - tanh((r - r3)*k(2))**2)*k(1)*k(2) + (1 - tanh(( &
      r - r_maxC)*k(2))**2)*k(1)*k(2))/((kappaC - one)*(one + tanh((r - &
      r2)*k(2))*k(1) + tanh((r - r3)*k(2))*k(1) + tanh((r - r_maxC)*k(2 &
      ))*k(1) + tanh((r2 - r_maxC)*k(2))*k(1) + tanh((r3 - r_maxC)*k(2))* &
      k(1))))/r)*sin((r - r_maxC)*k(5))
S_A23=0
S_A24=i*mC*(cos((r - r_maxC)*k(7)) - 1)/r
S_A31=(cos((r - r_maxC)*k(3))*k(3) + (r*two*((1 - tanh((r - r2)*k(2))**2)*k(1)* &
      k(2) + (1 - tanh((r - r3)*k(2))**2)*k(1)*k(2) + (1 - tanh((r - &
      r_maxC)*k(2))**2)*k(1)*k(2))/((kappaC - one)*(one + tanh((r - r2)*k &
      (2))*k(1) + tanh((r - r3)*k(2))*k(1) + tanh((r - r_maxC)*k(2))*k(1 &
      ) + tanh((r2 - r_maxC)*k(2))*k(1) + tanh((r3 - r_maxC)*k(2))*k(1 &
      ))))**((1.0d0/2.0d0)*two)*(kappaC - one)*sin((r - r_maxC)*k(3))/(r* &
      two))*(cos((r - r_maxC)*k(4)) - 1)
S_A32=0
S_A33=-i*(ak/(one + tanh((r - r2)*k(2))*k(1) + tanh((r - r3)*k(2))*k(1) + tanh &
      ((r - r_maxC)*k(2))*k(1) + tanh((r2 - r_maxC)*k(2))*k(1) + tanh((r3 &
      - r_maxC)*k(2))*k(1)) - m*sqrt(r*two*((1 - tanh((r - r2)*k(2))**2) &
      *k(1)*k(2) + (1 - tanh((r - r3)*k(2))**2)*k(1)*k(2) + (1 - tanh(( &
      r - r_maxC)*k(2))**2)*k(1)*k(2))/((kappaC - one)*(one + tanh((r - &
      r2)*k(2))*k(1) + tanh((r - r3)*k(2))*k(1) + tanh((r - r_maxC)*k(2 &
      ))*k(1) + tanh((r2 - r_maxC)*k(2))*k(1) + tanh((r3 - r_maxC)*k(2))* &
      k(1))))/r)*sin((r - r_maxC)*k(6))
S_A34=0
if (r.eq.1.0_rDef .or. r.eq.0.0_rDef) then
S_A41 = 0.0_rDef
else
S_A41=(cos((r - r_maxC)*k(4)) - 1)*(one/r - sin((r - r_maxC)*k(4))*k(4)/(cos((r &
      - r_maxC)*k(4)) - 1) + (r*two*((1 - tanh((r - r2)*k(2))**2)*k(1)*k &
      (2) + (1 - tanh((r - r3)*k(2))**2)*k(1)*k(2) + (1 - tanh((r - &
      r_maxC)*k(2))**2)*k(1)*k(2))/((kappaC - one)*(one + tanh((r - r2)*k &
      (2))*k(1) + tanh((r - r3)*k(2))*k(1) + tanh((r - r_maxC)*k(2))*k(1 &
      ) + tanh((r2 - r_maxC)*k(2))*k(1) + tanh((r3 - r_maxC)*k(2))*k(1 &
      ))))**((1.0d0/2.0d0)*two)*(kappaC + one)/(r*two))
endif
S_A42=i*mC*sin((r - r_maxC)*k(5))/r
S_A43=0
S_A44=-i*(ak/(one + tanh((r - r2)*k(2))*k(1) + tanh((r - r3)*k(2))*k(1) + tanh &
      ((r - r_maxC)*k(2))*k(1) + tanh((r2 - r_maxC)*k(2))*k(1) + tanh((r3 &
      - r_maxC)*k(2))*k(1)) - m*sqrt(r*two*((1 - tanh((r - r2)*k(2))**2) &
      *k(1)*k(2) + (1 - tanh((r - r3)*k(2))**2)*k(1)*k(2) + (1 - tanh(( &
      r - r_maxC)*k(2))**2)*k(1)*k(2))/((kappaC - one)*(one + tanh((r - &
      r2)*k(2))*k(1) + tanh((r - r3)*k(2))*k(1) + tanh((r - r_maxC)*k(2 &
      ))*k(1) + tanh((r2 - r_maxC)*k(2))*k(1) + tanh((r3 - r_maxC)*k(2))* &
      k(1))))/r)*(cos((r - r_maxC)*k(7)) - 1)
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


    ! if (r.eq.1.0_rDef) then
    !     S_A14 = CMPLX(0.0_rDef,KIND=rDef)
    !     S_A41 = CMPLX(0.0_rDef,KIND=rDef)
    ! ! elseif (r.eq.0.20_rDef) then
    !     ! S_A14 = CMPLX(0.0_rDef,KIND=rDef)
    !     ! S_A41 = CMPLX(0.0_rDef,KIND=rDef)
    ! ! else
    ! endif

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
