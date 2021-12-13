 
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
    S_1 = -i*(ak/(0.001d0*tanh(10.0d0*r - 10.0d0) + 0.001d0*tanh(10.0d0*r - 7.5d0 &
      ) + 0.001d0*tanh(10.0d0*r - 5.0d0) + 0.998013476497586d0) - gam &
      *(0.00833333333333333d0*tanh(125.0d0*r - 125.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 100.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 75.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 50.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 25.0d0) + &
      0.166666666666667d0) - 1.58113883008419d0*mC*sqrt(r*( &
      -0.0200398095526603d0*tanh(10.0d0*r - 10.0d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 7.5d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 5.0d0)**2 + &
      0.0601194286579808d0)/(0.00100199047763301d0*tanh(10.0d0*r - &
      10.0d0) + 0.00100199047763301d0*tanh(10.0d0*r - 7.5d0) + &
      0.00100199047763301d0*tanh(10.0d0*r - 5.0d0) + 1))/r)*(0.875d0*r &
      + 2.74658203125d0*(r - 0.2d0)**5 - 3.41796875d0*(r - 0.2d0)**4 - &
      0.390624999999998d0*(r - 0.2d0)**3 - 6.25d0*tanh(125.0d0*r - &
      125.0d0)**2 - 6.25d0*tanh(125.0d0*r - 91.666666666666671d0)**2 - &
      6.25d0*tanh(125.0d0*r - 58.333333333333336d0)**2 - 6.25d0*tanh( &
      125.0d0*r - 25.0d0)**2 + 24.825d0) - 6.25d0*(250.0d0 - 250.0d0* &
      tanh(125.0d0*r - 125.0d0)**2)*tanh(125.0d0*r - 125.0d0) - 6.25d0* &
      (250.0d0 - 250.0d0*tanh(125.0d0*r - 91.666666666666671d0)**2)* &
      tanh(125.0d0*r - 91.666666666666671d0) - 6.25d0*(250.0d0 - &
      250.0d0*tanh(125.0d0*r - 58.333333333333336d0)**2)*tanh(125.0d0*r &
      - 58.333333333333336d0) - 6.25d0*(250.0d0 - 250.0d0*tanh(125.0d0* &
      r - 25.0d0)**2)*tanh(125.0d0*r - 25.0d0) + 13.73291015625d0*(r - &
      0.2d0)**4 - 13.671875d0*(r - 0.2d0)**3 - 1.17187499999999d0*(r - &
      0.2d0)**2 + 0.875d0 - 1.58113883008419d0*two*sqrt(r*( &
      -0.0200398095526603d0*tanh(10.0d0*r - 10.0d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 7.5d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 5.0d0)**2 + &
      0.0601194286579808d0)/(0.00100199047763301d0*tanh(10.0d0*r - &
      10.0d0) + 0.00100199047763301d0*tanh(10.0d0*r - 7.5d0) + &
      0.00100199047763301d0*tanh(10.0d0*r - 5.0d0) + 1))*( &
      0.0333333333333333d0*tanh(18.75d0*r - 18.75d0) + &
      0.0333333333333333d0*tanh(18.75d0*r - 15.0d0) + &
      0.0333333333333333d0*tanh(18.75d0*r - 11.25d0) + &
      0.0333333333333333d0*tanh(18.75d0*r - 7.5d0) + &
      0.0333333333333333d0*tanh(18.75d0*r - 3.75d0) + &
      0.866703538980562d0)/r + (1.58113883008419d0*sqrt(r*( &
      -0.0200398095526603d0*tanh(10.0d0*r - 10.0d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 7.5d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 5.0d0)**2 + &
      0.0601194286579808d0)/(0.00100199047763301d0*tanh(10.0d0*r - &
      10.0d0) + 0.00100199047763301d0*tanh(10.0d0*r - 7.5d0) + &
      0.00100199047763301d0*tanh(10.0d0*r - 5.0d0) + 1)))**two*(kappaC - &
      one)*(0.875d0*r + 2.74658203125d0*(r - 0.2d0)**5 - 3.41796875d0*( &
      r - 0.2d0)**4 - 0.390624999999998d0*(r - 0.2d0)**3 - 6.25d0*tanh( &
      125.0d0*r - 125.0d0)**2 - 6.25d0*tanh(125.0d0*r - &
      91.666666666666671d0)**2 - 6.25d0*tanh(125.0d0*r - &
      58.333333333333336d0)**2 - 6.25d0*tanh(125.0d0*r - 25.0d0)**2 + &
      24.825d0)/r
    S_2 = i*mC*(0.875d0*r + 2.74658203125d0*(r - 0.2d0)**5 - 3.41796875d0*(r - &
      0.2d0)**4 - 0.390624999999998d0*(r - 0.2d0)**3 - 6.25d0*tanh( &
      125.0d0*r - 125.0d0)**2 - 6.25d0*tanh(125.0d0*r - &
      91.666666666666671d0)**2 - 6.25d0*tanh(125.0d0*r - &
      58.333333333333336d0)**2 - 6.25d0*tanh(125.0d0*r - 25.0d0)**2 + &
      24.825d0)/r - i*(ak/(0.001d0*tanh(10.0d0*r - 10.0d0) + 0.001d0* &
      tanh(10.0d0*r - 7.5d0) + 0.001d0*tanh(10.0d0*r - 5.0d0) + &
      0.998013476497586d0) - gam*(0.00833333333333333d0*tanh(125.0d0* &
      r - 125.0d0) + 0.00833333333333333d0*tanh(125.0d0*r - 100.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 75.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 50.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 25.0d0) + &
      0.166666666666667d0) - 1.58113883008419d0*mC*sqrt(r*( &
      -0.0200398095526603d0*tanh(10.0d0*r - 10.0d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 7.5d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 5.0d0)**2 + &
      0.0601194286579808d0)/(0.00100199047763301d0*tanh(10.0d0*r - &
      10.0d0) + 0.00100199047763301d0*tanh(10.0d0*r - 7.5d0) + &
      0.00100199047763301d0*tanh(10.0d0*r - 5.0d0) + 1))/r)*( &
      0.0333333333333333d0*tanh(18.75d0*r - 18.75d0) + &
      0.0333333333333333d0*tanh(18.75d0*r - 15.0d0) + &
      0.0333333333333333d0*tanh(18.75d0*r - 11.25d0) + &
      0.0333333333333333d0*tanh(18.75d0*r - 7.5d0) + &
      0.0333333333333333d0*tanh(18.75d0*r - 3.75d0) + &
      0.866703538980562d0) + (1.58113883008419d0*sqrt(r*( &
      -0.0200398095526603d0*tanh(10.0d0*r - 10.0d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 7.5d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 5.0d0)**2 + &
      0.0601194286579808d0)/(0.00100199047763301d0*tanh(10.0d0*r - &
      10.0d0) + 0.00100199047763301d0*tanh(10.0d0*r - 7.5d0) + &
      0.00100199047763301d0*tanh(10.0d0*r - 5.0d0) + 1))*(0.5d0*r*( &
      -0.0200398095526603d0*(20 - 20*tanh(10.0d0*r - 10.0d0)**2)*tanh( &
      10.0d0*r - 10.0d0) - 0.0200398095526603d0*(20 - 20*tanh(10.0d0*r &
      - 7.5d0)**2)*tanh(10.0d0*r - 7.5d0) - 0.0200398095526603d0*(20 - &
      20*tanh(10.0d0*r - 5.0d0)**2)*tanh(10.0d0*r - 5.0d0))/( &
      0.00100199047763301d0*tanh(10.0d0*r - 10.0d0) + &
      0.00100199047763301d0*tanh(10.0d0*r - 7.5d0) + &
      0.00100199047763301d0*tanh(10.0d0*r - 5.0d0) + 1) + 0.5d0*r*( &
      -0.0200398095526603d0*tanh(10.0d0*r - 10.0d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 7.5d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 5.0d0)**2 + &
      0.0601194286579808d0)*(0.0100199047763301d0*tanh(10.0d0*r - &
      10.0d0)**2 + 0.0100199047763301d0*tanh(10.0d0*r - 7.5d0)**2 + &
      0.0100199047763301d0*tanh(10.0d0*r - 5.0d0)**2 - &
      0.0300597143289904d0)/(0.00100199047763301d0*tanh(10.0d0*r - &
      10.0d0) + 0.00100199047763301d0*tanh(10.0d0*r - 7.5d0) + &
      0.00100199047763301d0*tanh(10.0d0*r - 5.0d0) + 1)**2 + 0.5d0*( &
      -0.0200398095526603d0*tanh(10.0d0*r - 10.0d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 7.5d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 5.0d0)**2 + &
      0.0601194286579808d0)/(0.00100199047763301d0*tanh(10.0d0*r - &
      10.0d0) + 0.00100199047763301d0*tanh(10.0d0*r - 7.5d0) + &
      0.00100199047763301d0*tanh(10.0d0*r - 5.0d0) + 1))*( &
      0.00100199047763301d0*tanh(10.0d0*r - 10.0d0) + &
      0.00100199047763301d0*tanh(10.0d0*r - 7.5d0) + &
      0.00100199047763301d0*tanh(10.0d0*r - 5.0d0) + 1)/(r*( &
      -0.0200398095526603d0*tanh(10.0d0*r - 10.0d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 7.5d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 5.0d0)**2 + &
      0.0601194286579808d0)) + 1.58113883008419d0*sqrt(r*( &
      -0.0200398095526603d0*tanh(10.0d0*r - 10.0d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 7.5d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 5.0d0)**2 + &
      0.0601194286579808d0)/(0.00100199047763301d0*tanh(10.0d0*r - &
      10.0d0) + 0.00100199047763301d0*tanh(10.0d0*r - 7.5d0) + &
      0.00100199047763301d0*tanh(10.0d0*r - 5.0d0) + 1))/r + ( &
      1.58113883008419d0*sqrt(r*(-0.0200398095526603d0*tanh(10.0d0*r - &
      10.0d0)**2 - 0.0200398095526603d0*tanh(10.0d0*r - 7.5d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 5.0d0)**2 + &
      0.0601194286579808d0)/(0.00100199047763301d0*tanh(10.0d0*r - &
      10.0d0) + 0.00100199047763301d0*tanh(10.0d0*r - 7.5d0) + &
      0.00100199047763301d0*tanh(10.0d0*r - 5.0d0) + 1)))**three*(kappaC &
      - one)/(r*two))*(0.875d0*r + 2.74658203125d0*(r - 0.2d0)**5 - &
      3.41796875d0*(r - 0.2d0)**4 - 0.390624999999998d0*(r - 0.2d0)**3 &
      - 6.25d0*tanh(125.0d0*r - 125.0d0)**2 - 6.25d0*tanh(125.0d0*r - &
      91.666666666666671d0)**2 - 6.25d0*tanh(125.0d0*r - &
      58.333333333333336d0)**2 - 6.25d0*tanh(125.0d0*r - 25.0d0)**2 + &
      24.825d0)
    S_3 = gam*i*one*(0.875d0*r + 2.74658203125d0*(r - 0.2d0)**5 - 3.41796875d0*( &
      r - 0.2d0)**4 - 0.390624999999998d0*(r - 0.2d0)**3 - 6.25d0*tanh( &
      125.0d0*r - 125.0d0)**2 - 6.25d0*tanh(125.0d0*r - &
      91.666666666666671d0)**2 - 6.25d0*tanh(125.0d0*r - &
      58.333333333333336d0)**2 - 6.25d0*tanh(125.0d0*r - 25.0d0)**2 + &
      24.825d0) - i*(ak/(0.001d0*tanh(10.0d0*r - 10.0d0) + 0.001d0*tanh &
      (10.0d0*r - 7.5d0) + 0.001d0*tanh(10.0d0*r - 5.0d0) + &
      0.998013476497586d0) - gam*(0.00833333333333333d0*tanh(125.0d0* &
      r - 125.0d0) + 0.00833333333333333d0*tanh(125.0d0*r - 100.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 75.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 50.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 25.0d0) + &
      0.166666666666667d0) - 1.58113883008419d0*mC*sqrt(r*( &
      -0.0200398095526603d0*tanh(10.0d0*r - 10.0d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 7.5d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 5.0d0)**2 + &
      0.0601194286579808d0)/(0.00100199047763301d0*tanh(10.0d0*r - &
      10.0d0) + 0.00100199047763301d0*tanh(10.0d0*r - 7.5d0) + &
      0.00100199047763301d0*tanh(10.0d0*r - 5.0d0) + 1))/r)*(0.1d0*tanh &
      (25.0d0*r - 25.0d0) + 0.1d0*tanh(25.0d0*r - 18.333333333333336d0 &
      ) + 0.1d0*tanh(25.0d0*r - 11.666666666666668d0) + 0.1d0*tanh( &
      25.0d0*r - 5.0d0) + 0.700000323919358d0) + (-1.04166666666667d0* &
      tanh(125.0d0*r - 125.0d0)**2 - 1.04166666666667d0*tanh(125.0d0*r &
      - 100.0d0)**2 - 1.04166666666667d0*tanh(125.0d0*r - 75.0d0)**2 - &
      1.04166666666667d0*tanh(125.0d0*r - 50.0d0)**2 - &
      1.04166666666667d0*tanh(125.0d0*r - 25.0d0)**2 + &
      5.20833333333333d0 + (1.58113883008419d0*sqrt(r*( &
      -0.0200398095526603d0*tanh(10.0d0*r - 10.0d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 7.5d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 5.0d0)**2 + &
      0.0601194286579808d0)/(0.00100199047763301d0*tanh(10.0d0*r - &
      10.0d0) + 0.00100199047763301d0*tanh(10.0d0*r - 7.5d0) + &
      0.00100199047763301d0*tanh(10.0d0*r - 5.0d0) + 1)))**two*(kappaC - &
      one)*(0.00833333333333333d0*tanh(125.0d0*r - 125.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 100.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 75.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 50.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 25.0d0) + &
      0.166666666666667d0)/(r*two))*(0.875d0*r + 2.74658203125d0*(r - &
      0.2d0)**5 - 3.41796875d0*(r - 0.2d0)**4 - 0.390624999999998d0*(r &
      - 0.2d0)**3 - 6.25d0*tanh(125.0d0*r - 125.0d0)**2 - 6.25d0*tanh( &
      125.0d0*r - 91.666666666666671d0)**2 - 6.25d0*tanh(125.0d0*r - &
      58.333333333333336d0)**2 - 6.25d0*tanh(125.0d0*r - 25.0d0)**2 + &
      24.825d0)
    S_4 = gam*i*one*(0.1d0*tanh(25.0d0*r - 25.0d0) + 0.1d0*tanh(25.0d0*r - &
      18.333333333333336d0) + 0.1d0*tanh(25.0d0*r - &
      11.666666666666668d0) + 0.1d0*tanh(25.0d0*r - 5.0d0) + &
      0.700000323919358d0) + i*mC*(0.0333333333333333d0*tanh(18.75d0*r - &
      18.75d0) + 0.0333333333333333d0*tanh(18.75d0*r - 15.0d0) + &
      0.0333333333333333d0*tanh(18.75d0*r - 11.25d0) + &
      0.0333333333333333d0*tanh(18.75d0*r - 7.5d0) + &
      0.0333333333333333d0*tanh(18.75d0*r - 3.75d0) + &
      0.866703538980562d0)/r - i*(ak/(0.001d0*tanh(10.0d0*r - 10.0d0) + &
      0.001d0*tanh(10.0d0*r - 7.5d0) + 0.001d0*tanh(10.0d0*r - 5.0d0) + &
      0.998013476497586d0) - gam*(0.00833333333333333d0*tanh(125.0d0* &
      r - 125.0d0) + 0.00833333333333333d0*tanh(125.0d0*r - 100.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 75.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 50.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 25.0d0) + &
      0.166666666666667d0) - 1.58113883008419d0*mC*sqrt(r*( &
      -0.0200398095526603d0*tanh(10.0d0*r - 10.0d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 7.5d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 5.0d0)**2 + &
      0.0601194286579808d0)/(0.00100199047763301d0*tanh(10.0d0*r - &
      10.0d0) + 0.00100199047763301d0*tanh(10.0d0*r - 7.5d0) + &
      0.00100199047763301d0*tanh(10.0d0*r - 5.0d0) + 1))/r)*(0.875d0*r &
      + 2.74658203125d0*(r - 0.2d0)**5 - 3.41796875d0*(r - 0.2d0)**4 - &
      0.390624999999998d0*(r - 0.2d0)**3 - 6.25d0*tanh(125.0d0*r - &
      125.0d0)**2 - 6.25d0*tanh(125.0d0*r - 91.666666666666671d0)**2 - &
      6.25d0*tanh(125.0d0*r - 58.333333333333336d0)**2 - 6.25d0*tanh( &
      125.0d0*r - 25.0d0)**2 + 24.825d0) - 6.25d0*(250.0d0 - 250.0d0* &
      tanh(125.0d0*r - 125.0d0)**2)*tanh(125.0d0*r - 125.0d0) - 6.25d0* &
      (250.0d0 - 250.0d0*tanh(125.0d0*r - 91.666666666666671d0)**2)* &
      tanh(125.0d0*r - 91.666666666666671d0) - 6.25d0*(250.0d0 - &
      250.0d0*tanh(125.0d0*r - 58.333333333333336d0)**2)*tanh(125.0d0*r &
      - 58.333333333333336d0) - 6.25d0*(250.0d0 - 250.0d0*tanh(125.0d0* &
      r - 25.0d0)**2)*tanh(125.0d0*r - 25.0d0) + 13.73291015625d0*(r - &
      0.2d0)**4 - 13.671875d0*(r - 0.2d0)**3 - 1.17187499999999d0*(r - &
      0.2d0)**2 + (one/r + (1.58113883008419d0*sqrt(r*( &
      -0.0200398095526603d0*tanh(10.0d0*r - 10.0d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 7.5d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 5.0d0)**2 + &
      0.0601194286579808d0)/(0.00100199047763301d0*tanh(10.0d0*r - &
      10.0d0) + 0.00100199047763301d0*tanh(10.0d0*r - 7.5d0) + &
      0.00100199047763301d0*tanh(10.0d0*r - 5.0d0) + 1)))**two*(kappaC + &
      one)/(r*two))*(0.875d0*r + 2.74658203125d0*(r - 0.2d0)**5 - &
      3.41796875d0*(r - 0.2d0)**4 - 0.390624999999998d0*(r - 0.2d0)**3 &
      - 6.25d0*tanh(125.0d0*r - 125.0d0)**2 - 6.25d0*tanh(125.0d0*r - &
      91.666666666666671d0)**2 - 6.25d0*tanh(125.0d0*r - &
      58.333333333333336d0)**2 - 6.25d0*tanh(125.0d0*r - 25.0d0)**2 + &
      24.825d0) + 0.875d0

    END SUBROUTINE SourceCalc
