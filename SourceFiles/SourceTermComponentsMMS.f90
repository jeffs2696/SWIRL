 
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
S_A11=-i*(ak/(0.1d0*tanh(0.10000000000000001d0*r - 0.10000000000000001d0) + &
      0.1d0*tanh(0.10000000000000001d0*r - 0.075000000000000011d0) + &
      0.1d0*tanh(0.10000000000000001d0*r - 0.050000000000000003d0) + &
      0.99250468320737d0) - 1.58113883008419d0*mC*sqrt(r*( &
      -0.0201510384166331d0*tanh(0.10000000000000001d0*r - &
      0.10000000000000001d0)**2 - 0.0201510384166331d0*tanh( &
      0.10000000000000001d0*r - 0.075000000000000011d0)**2 - &
      0.0201510384166331d0*tanh(0.10000000000000001d0*r - &
      0.050000000000000003d0)**2 + 0.0604531152498994d0)/( &
      0.100755192083166d0*tanh(0.10000000000000001d0*r - &
      0.10000000000000001d0) + 0.100755192083166d0*tanh( &
      0.10000000000000001d0*r - 0.075000000000000011d0) + &
      0.100755192083166d0*tanh(0.10000000000000001d0*r - &
      0.050000000000000003d0) + 1))/r)*(0.763888888888943d0*r + &
      3.56038411458294d0*(r - 0.2d0)**5 - 5.15407986111027d0*(r - 0.2d0 &
      )**4 + 0.651041666666159d0*(r - 0.2d0)**3 - 3.47222222222222d0* &
      tanh(125.0d0*r - 125.0d0)**2 - 3.47222222222222d0*tanh(125.0d0*r &
      - 110.71428571428572d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      96.428571428571431d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      82.142857142857139d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      67.857142857142861d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      53.571428571428584d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      39.285714285714292d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      25.0d0)**2 + 27.625d0)
S_A12=-1.58113883008419d0*two*sqrt(r*(-0.0201510384166331d0*tanh( &
      0.10000000000000001d0*r - 0.10000000000000001d0)**2 - &
      0.0201510384166331d0*tanh(0.10000000000000001d0*r - &
      0.075000000000000011d0)**2 - 0.0201510384166331d0*tanh( &
      0.10000000000000001d0*r - 0.050000000000000003d0)**2 + &
      0.0604531152498994d0)/(0.100755192083166d0*tanh( &
      0.10000000000000001d0*r - 0.10000000000000001d0) + &
      0.100755192083166d0*tanh(0.10000000000000001d0*r - &
      0.075000000000000011d0) + 0.100755192083166d0*tanh( &
      0.10000000000000001d0*r - 0.050000000000000003d0) + 1))*( &
      1.51909722222205d0*(r - 0.2d0)**3 - 1.82291666666646d0*(r - 0.2d0 &
      )**2 + 0.0277777777777778d0*tanh(125.0d0*r - 125.0d0) + &
      0.0277777777777778d0*tanh(125.0d0*r - 110.71428571428572d0) + &
      0.0277777777777778d0*tanh(125.0d0*r - 96.428571428571431d0) + &
      0.0277777777777778d0*tanh(125.0d0*r - 82.142857142857139d0) + &
      0.0277777777777778d0*tanh(125.0d0*r - 67.857142857142861d0) + &
      0.0277777777777778d0*tanh(125.0d0*r - 53.571428571428584d0) + &
      0.0277777777777778d0*tanh(125.0d0*r - 39.285714285714292d0) + &
      0.0277777777777778d0*tanh(125.0d0*r - 25.0d0) + &
      0.194444444444423d0)/r
S_A13=0
S_A14=((-3.47222222222222d0*(250.0d0 - 250.0d0*tanh(125.0d0*r - 125.0d0)**2)* &
      tanh(125.0d0*r - 125.0d0) - 3.47222222222222d0*(250.0d0 - 250.0d0 &
      *tanh(125.0d0*r - 110.71428571428572d0)**2)*tanh(125.0d0*r - &
      110.71428571428572d0) - 3.47222222222222d0*(250.0d0 - 250.0d0* &
      tanh(125.0d0*r - 96.428571428571431d0)**2)*tanh(125.0d0*r - &
      96.428571428571431d0) - 3.47222222222222d0*(250.0d0 - 250.0d0* &
      tanh(125.0d0*r - 82.142857142857139d0)**2)*tanh(125.0d0*r - &
      82.142857142857139d0) - 3.47222222222222d0*(250.0d0 - 250.0d0* &
      tanh(125.0d0*r - 67.857142857142861d0)**2)*tanh(125.0d0*r - &
      67.857142857142861d0) - 3.47222222222222d0*(250.0d0 - 250.0d0* &
      tanh(125.0d0*r - 53.571428571428584d0)**2)*tanh(125.0d0*r - &
      53.571428571428584d0) - 3.47222222222222d0*(250.0d0 - 250.0d0* &
      tanh(125.0d0*r - 39.285714285714292d0)**2)*tanh(125.0d0*r - &
      39.285714285714292d0) - 3.47222222222222d0*(250.0d0 - 250.0d0* &
      tanh(125.0d0*r - 25.0d0)**2)*tanh(125.0d0*r - 25.0d0) + &
      17.8019205729147d0*(r - 0.2d0)**4 - 20.6163194444411d0*(r - 0.2d0 &
      )**3 + 1.95312499999848d0*(r - 0.2d0)**2 + 0.763888888888943d0)/( &
      0.763888888888943d0*r + 3.56038411458294d0*(r - 0.2d0)**5 - &
      5.15407986111027d0*(r - 0.2d0)**4 + 0.651041666666159d0*(r - &
      0.2d0)**3 - 3.47222222222222d0*tanh(125.0d0*r - 125.0d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 110.71428571428572d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 96.428571428571431d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 82.142857142857139d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 67.857142857142861d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 53.571428571428584d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 39.285714285714292d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 25.0d0)**2 + 27.625d0) + ( &
      1.58113883008419d0*sqrt(r*(-0.0201510384166331d0*tanh( &
      0.10000000000000001d0*r - 0.10000000000000001d0)**2 - &
      0.0201510384166331d0*tanh(0.10000000000000001d0*r - &
      0.075000000000000011d0)**2 - 0.0201510384166331d0*tanh( &
      0.10000000000000001d0*r - 0.050000000000000003d0)**2 + &
      0.0604531152498994d0)/(0.100755192083166d0*tanh( &
      0.10000000000000001d0*r - 0.10000000000000001d0) + &
      0.100755192083166d0*tanh(0.10000000000000001d0*r - &
      0.075000000000000011d0) + 0.100755192083166d0*tanh( &
      0.10000000000000001d0*r - 0.050000000000000003d0) + 1)))**two*( &
      kappaC - one)/r)*(0.763888888888943d0*r + 3.56038411458294d0*(r - &
      0.2d0)**5 - 5.15407986111027d0*(r - 0.2d0)**4 + &
      0.651041666666159d0*(r - 0.2d0)**3 - 3.47222222222222d0*tanh( &
      125.0d0*r - 125.0d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      110.71428571428572d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      96.428571428571431d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      82.142857142857139d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      67.857142857142861d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      53.571428571428584d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      39.285714285714292d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      25.0d0)**2 + 27.625d0)
S_A21=(1.58113883008419d0*sqrt(r*(-0.0201510384166331d0*tanh( &
      0.10000000000000001d0*r - 0.10000000000000001d0)**2 - &
      0.0201510384166331d0*tanh(0.10000000000000001d0*r - &
      0.075000000000000011d0)**2 - 0.0201510384166331d0*tanh( &
      0.10000000000000001d0*r - 0.050000000000000003d0)**2 + &
      0.0604531152498994d0)/(0.100755192083166d0*tanh( &
      0.10000000000000001d0*r - 0.10000000000000001d0) + &
      0.100755192083166d0*tanh(0.10000000000000001d0*r - &
      0.075000000000000011d0) + 0.100755192083166d0*tanh( &
      0.10000000000000001d0*r - 0.050000000000000003d0) + 1))*(0.5d0*r* &
      (-0.0201510384166331d0*(0.2d0 - 0.2d0*tanh(0.10000000000000001d0* &
      r - 0.10000000000000001d0)**2)*tanh(0.10000000000000001d0*r - &
      0.10000000000000001d0) - 0.0201510384166331d0*(0.2d0 - 0.2d0*tanh &
      (0.10000000000000001d0*r - 0.075000000000000011d0)**2)*tanh( &
      0.10000000000000001d0*r - 0.075000000000000011d0) - &
      0.0201510384166331d0*(0.2d0 - 0.2d0*tanh(0.10000000000000001d0*r &
      - 0.050000000000000003d0)**2)*tanh(0.10000000000000001d0*r - &
      0.050000000000000003d0))/(0.100755192083166d0*tanh( &
      0.10000000000000001d0*r - 0.10000000000000001d0) + &
      0.100755192083166d0*tanh(0.10000000000000001d0*r - &
      0.075000000000000011d0) + 0.100755192083166d0*tanh( &
      0.10000000000000001d0*r - 0.050000000000000003d0) + 1) + 0.5d0*r* &
      (-0.0201510384166331d0*tanh(0.10000000000000001d0*r - &
      0.10000000000000001d0)**2 - 0.0201510384166331d0*tanh( &
      0.10000000000000001d0*r - 0.075000000000000011d0)**2 - &
      0.0201510384166331d0*tanh(0.10000000000000001d0*r - &
      0.050000000000000003d0)**2 + 0.0604531152498994d0)*( &
      0.0100755192083166d0*tanh(0.10000000000000001d0*r - &
      0.10000000000000001d0)**2 + 0.0100755192083166d0*tanh( &
      0.10000000000000001d0*r - 0.075000000000000011d0)**2 + &
      0.0100755192083166d0*tanh(0.10000000000000001d0*r - &
      0.050000000000000003d0)**2 - 0.0302265576249497d0)/( &
      0.100755192083166d0*tanh(0.10000000000000001d0*r - &
      0.10000000000000001d0) + 0.100755192083166d0*tanh( &
      0.10000000000000001d0*r - 0.075000000000000011d0) + &
      0.100755192083166d0*tanh(0.10000000000000001d0*r - &
      0.050000000000000003d0) + 1)**2 + 0.5d0*(-0.0201510384166331d0* &
      tanh(0.10000000000000001d0*r - 0.10000000000000001d0)**2 - &
      0.0201510384166331d0*tanh(0.10000000000000001d0*r - &
      0.075000000000000011d0)**2 - 0.0201510384166331d0*tanh( &
      0.10000000000000001d0*r - 0.050000000000000003d0)**2 + &
      0.0604531152498994d0)/(0.100755192083166d0*tanh( &
      0.10000000000000001d0*r - 0.10000000000000001d0) + &
      0.100755192083166d0*tanh(0.10000000000000001d0*r - &
      0.075000000000000011d0) + 0.100755192083166d0*tanh( &
      0.10000000000000001d0*r - 0.050000000000000003d0) + 1))*( &
      0.100755192083166d0*tanh(0.10000000000000001d0*r - &
      0.10000000000000001d0) + 0.100755192083166d0*tanh( &
      0.10000000000000001d0*r - 0.075000000000000011d0) + &
      0.100755192083166d0*tanh(0.10000000000000001d0*r - &
      0.050000000000000003d0) + 1)/(r*(-0.0201510384166331d0*tanh( &
      0.10000000000000001d0*r - 0.10000000000000001d0)**2 - &
      0.0201510384166331d0*tanh(0.10000000000000001d0*r - &
      0.075000000000000011d0)**2 - 0.0201510384166331d0*tanh( &
      0.10000000000000001d0*r - 0.050000000000000003d0)**2 + &
      0.0604531152498994d0)) + 1.58113883008419d0*sqrt(r*( &
      -0.0201510384166331d0*tanh(0.10000000000000001d0*r - &
      0.10000000000000001d0)**2 - 0.0201510384166331d0*tanh( &
      0.10000000000000001d0*r - 0.075000000000000011d0)**2 - &
      0.0201510384166331d0*tanh(0.10000000000000001d0*r - &
      0.050000000000000003d0)**2 + 0.0604531152498994d0)/( &
      0.100755192083166d0*tanh(0.10000000000000001d0*r - &
      0.10000000000000001d0) + 0.100755192083166d0*tanh( &
      0.10000000000000001d0*r - 0.075000000000000011d0) + &
      0.100755192083166d0*tanh(0.10000000000000001d0*r - &
      0.050000000000000003d0) + 1))/r + (1.58113883008419d0*sqrt(r*( &
      -0.0201510384166331d0*tanh(0.10000000000000001d0*r - &
      0.10000000000000001d0)**2 - 0.0201510384166331d0*tanh( &
      0.10000000000000001d0*r - 0.075000000000000011d0)**2 - &
      0.0201510384166331d0*tanh(0.10000000000000001d0*r - &
      0.050000000000000003d0)**2 + 0.0604531152498994d0)/( &
      0.100755192083166d0*tanh(0.10000000000000001d0*r - &
      0.10000000000000001d0) + 0.100755192083166d0*tanh( &
      0.10000000000000001d0*r - 0.075000000000000011d0) + &
      0.100755192083166d0*tanh(0.10000000000000001d0*r - &
      0.050000000000000003d0) + 1)))**three*(kappaC - one)/(r*two))*( &
      0.763888888888943d0*r + 3.56038411458294d0*(r - 0.2d0)**5 - &
      5.15407986111027d0*(r - 0.2d0)**4 + 0.651041666666159d0*(r - &
      0.2d0)**3 - 3.47222222222222d0*tanh(125.0d0*r - 125.0d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 110.71428571428572d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 96.428571428571431d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 82.142857142857139d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 67.857142857142861d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 53.571428571428584d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 39.285714285714292d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 25.0d0)**2 + 27.625d0)
S_A22=-i*(ak/(0.1d0*tanh(0.10000000000000001d0*r - 0.10000000000000001d0) + &
      0.1d0*tanh(0.10000000000000001d0*r - 0.075000000000000011d0) + &
      0.1d0*tanh(0.10000000000000001d0*r - 0.050000000000000003d0) + &
      0.99250468320737d0) - 1.58113883008419d0*mC*sqrt(r*( &
      -0.0201510384166331d0*tanh(0.10000000000000001d0*r - &
      0.10000000000000001d0)**2 - 0.0201510384166331d0*tanh( &
      0.10000000000000001d0*r - 0.075000000000000011d0)**2 - &
      0.0201510384166331d0*tanh(0.10000000000000001d0*r - &
      0.050000000000000003d0)**2 + 0.0604531152498994d0)/( &
      0.100755192083166d0*tanh(0.10000000000000001d0*r - &
      0.10000000000000001d0) + 0.100755192083166d0*tanh( &
      0.10000000000000001d0*r - 0.075000000000000011d0) + &
      0.100755192083166d0*tanh(0.10000000000000001d0*r - &
      0.050000000000000003d0) + 1))/r)*(1.51909722222205d0*(r - 0.2d0) &
      **3 - 1.82291666666646d0*(r - 0.2d0)**2 + 0.0277777777777778d0* &
      tanh(125.0d0*r - 125.0d0) + 0.0277777777777778d0*tanh(125.0d0*r - &
      110.71428571428572d0) + 0.0277777777777778d0*tanh(125.0d0*r - &
      96.428571428571431d0) + 0.0277777777777778d0*tanh(125.0d0*r - &
      82.142857142857139d0) + 0.0277777777777778d0*tanh(125.0d0*r - &
      67.857142857142861d0) + 0.0277777777777778d0*tanh(125.0d0*r - &
      53.571428571428584d0) + 0.0277777777777778d0*tanh(125.0d0*r - &
      39.285714285714292d0) + 0.0277777777777778d0*tanh(125.0d0*r - &
      25.0d0) + 0.194444444444423d0)
S_A23=0
S_A24=i*mC*(0.763888888888943d0*r + 3.56038411458294d0*(r - 0.2d0)**5 - &
      5.15407986111027d0*(r - 0.2d0)**4 + 0.651041666666159d0*(r - &
      0.2d0)**3 - 3.47222222222222d0*tanh(125.0d0*r - 125.0d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 110.71428571428572d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 96.428571428571431d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 82.142857142857139d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 67.857142857142861d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 53.571428571428584d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 39.285714285714292d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 25.0d0)**2 + 27.625d0)/r
S_A31=(-1.04166666666667d0*tanh(125.0d0*r - 125.0d0)**2 - 1.04166666666667d0* &
      tanh(125.0d0*r - 100.0d0)**2 - 1.04166666666667d0*tanh(125.0d0*r &
      - 75.0d0)**2 - 1.04166666666667d0*tanh(125.0d0*r - 50.0d0)**2 - &
      1.04166666666667d0*tanh(125.0d0*r - 25.0d0)**2 + &
      5.20833333333333d0 + (1.58113883008419d0*sqrt(r*( &
      -0.0201510384166331d0*tanh(0.10000000000000001d0*r - &
      0.10000000000000001d0)**2 - 0.0201510384166331d0*tanh( &
      0.10000000000000001d0*r - 0.075000000000000011d0)**2 - &
      0.0201510384166331d0*tanh(0.10000000000000001d0*r - &
      0.050000000000000003d0)**2 + 0.0604531152498994d0)/( &
      0.100755192083166d0*tanh(0.10000000000000001d0*r - &
      0.10000000000000001d0) + 0.100755192083166d0*tanh( &
      0.10000000000000001d0*r - 0.075000000000000011d0) + &
      0.100755192083166d0*tanh(0.10000000000000001d0*r - &
      0.050000000000000003d0) + 1)))**two*(kappaC - one)*( &
      0.00833333333333333d0*tanh(125.0d0*r - 125.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 100.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 75.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 50.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 25.0d0) + &
      0.166666666666667d0)/(r*two))*(0.763888888888943d0*r + &
      3.56038411458294d0*(r - 0.2d0)**5 - 5.15407986111027d0*(r - 0.2d0 &
      )**4 + 0.651041666666159d0*(r - 0.2d0)**3 - 3.47222222222222d0* &
      tanh(125.0d0*r - 125.0d0)**2 - 3.47222222222222d0*tanh(125.0d0*r &
      - 110.71428571428572d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      96.428571428571431d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      82.142857142857139d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      67.857142857142861d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      53.571428571428584d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      39.285714285714292d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      25.0d0)**2 + 27.625d0)
S_A32=0
S_A33=-i*(ak/(0.1d0*tanh(0.10000000000000001d0*r - 0.10000000000000001d0) + &
      0.1d0*tanh(0.10000000000000001d0*r - 0.075000000000000011d0) + &
      0.1d0*tanh(0.10000000000000001d0*r - 0.050000000000000003d0) + &
      0.99250468320737d0) - 1.58113883008419d0*mC*sqrt(r*( &
      -0.0201510384166331d0*tanh(0.10000000000000001d0*r - &
      0.10000000000000001d0)**2 - 0.0201510384166331d0*tanh( &
      0.10000000000000001d0*r - 0.075000000000000011d0)**2 - &
      0.0201510384166331d0*tanh(0.10000000000000001d0*r - &
      0.050000000000000003d0)**2 + 0.0604531152498994d0)/( &
      0.100755192083166d0*tanh(0.10000000000000001d0*r - &
      0.10000000000000001d0) + 0.100755192083166d0*tanh( &
      0.10000000000000001d0*r - 0.075000000000000011d0) + &
      0.100755192083166d0*tanh(0.10000000000000001d0*r - &
      0.050000000000000003d0) + 1))/r)*(0.0454545454545455d0*tanh(2.5d0 &
      *r - 2.5d0) + 0.0454545454545455d0*tanh(2.5d0*r - &
      2.2777777777777777d0) + 0.0454545454545455d0*tanh(2.5d0*r - &
      2.0555555555555554d0) + 0.0454545454545455d0*tanh(2.5d0*r - &
      1.8333333333333335d0) + 0.0454545454545455d0*tanh(2.5d0*r - &
      1.6111111111111112d0) + 0.0454545454545455d0*tanh(2.5d0*r - &
      1.3888888888888888d0) + 0.0454545454545455d0*tanh(2.5d0*r - &
      1.1666666666666667d0) + 0.0454545454545455d0*tanh(2.5d0*r - &
      0.94444444444444464d0) + 0.0454545454545455d0*tanh(2.5d0*r - &
      0.72222222222222232d0) + 0.0454545454545455d0*tanh(2.5d0*r - &
      0.5d0) + 0.707850846972511d0)
S_A34=0
S_A41=(one/r + (-3.47222222222222d0*(250.0d0 - 250.0d0*tanh(125.0d0*r - &
      125.0d0)**2)*tanh(125.0d0*r - 125.0d0) - 3.47222222222222d0*( &
      250.0d0 - 250.0d0*tanh(125.0d0*r - 110.71428571428572d0)**2)*tanh &
      (125.0d0*r - 110.71428571428572d0) - 3.47222222222222d0*(250.0d0 &
      - 250.0d0*tanh(125.0d0*r - 96.428571428571431d0)**2)*tanh(125.0d0 &
      *r - 96.428571428571431d0) - 3.47222222222222d0*(250.0d0 - &
      250.0d0*tanh(125.0d0*r - 82.142857142857139d0)**2)*tanh(125.0d0*r &
      - 82.142857142857139d0) - 3.47222222222222d0*(250.0d0 - 250.0d0* &
      tanh(125.0d0*r - 67.857142857142861d0)**2)*tanh(125.0d0*r - &
      67.857142857142861d0) - 3.47222222222222d0*(250.0d0 - 250.0d0* &
      tanh(125.0d0*r - 53.571428571428584d0)**2)*tanh(125.0d0*r - &
      53.571428571428584d0) - 3.47222222222222d0*(250.0d0 - 250.0d0* &
      tanh(125.0d0*r - 39.285714285714292d0)**2)*tanh(125.0d0*r - &
      39.285714285714292d0) - 3.47222222222222d0*(250.0d0 - 250.0d0* &
      tanh(125.0d0*r - 25.0d0)**2)*tanh(125.0d0*r - 25.0d0) + &
      17.8019205729147d0*(r - 0.2d0)**4 - 20.6163194444411d0*(r - 0.2d0 &
      )**3 + 1.95312499999848d0*(r - 0.2d0)**2 + 0.763888888888943d0)/( &
      0.763888888888943d0*r + 3.56038411458294d0*(r - 0.2d0)**5 - &
      5.15407986111027d0*(r - 0.2d0)**4 + 0.651041666666159d0*(r - &
      0.2d0)**3 - 3.47222222222222d0*tanh(125.0d0*r - 125.0d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 110.71428571428572d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 96.428571428571431d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 82.142857142857139d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 67.857142857142861d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 53.571428571428584d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 39.285714285714292d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 25.0d0)**2 + 27.625d0) + ( &
      1.58113883008419d0*sqrt(r*(-0.0201510384166331d0*tanh( &
      0.10000000000000001d0*r - 0.10000000000000001d0)**2 - &
      0.0201510384166331d0*tanh(0.10000000000000001d0*r - &
      0.075000000000000011d0)**2 - 0.0201510384166331d0*tanh( &
      0.10000000000000001d0*r - 0.050000000000000003d0)**2 + &
      0.0604531152498994d0)/(0.100755192083166d0*tanh( &
      0.10000000000000001d0*r - 0.10000000000000001d0) + &
      0.100755192083166d0*tanh(0.10000000000000001d0*r - &
      0.075000000000000011d0) + 0.100755192083166d0*tanh( &
      0.10000000000000001d0*r - 0.050000000000000003d0) + 1)))**two*( &
      kappaC + one)/(r*two))*(0.763888888888943d0*r + 3.56038411458294d0 &
      *(r - 0.2d0)**5 - 5.15407986111027d0*(r - 0.2d0)**4 + &
      0.651041666666159d0*(r - 0.2d0)**3 - 3.47222222222222d0*tanh( &
      125.0d0*r - 125.0d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      110.71428571428572d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      96.428571428571431d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      82.142857142857139d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      67.857142857142861d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      53.571428571428584d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      39.285714285714292d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      25.0d0)**2 + 27.625d0)
S_A42=i*mC*(1.51909722222205d0*(r - 0.2d0)**3 - 1.82291666666646d0*(r - 0.2d0) &
      **2 + 0.0277777777777778d0*tanh(125.0d0*r - 125.0d0) + &
      0.0277777777777778d0*tanh(125.0d0*r - 110.71428571428572d0) + &
      0.0277777777777778d0*tanh(125.0d0*r - 96.428571428571431d0) + &
      0.0277777777777778d0*tanh(125.0d0*r - 82.142857142857139d0) + &
      0.0277777777777778d0*tanh(125.0d0*r - 67.857142857142861d0) + &
      0.0277777777777778d0*tanh(125.0d0*r - 53.571428571428584d0) + &
      0.0277777777777778d0*tanh(125.0d0*r - 39.285714285714292d0) + &
      0.0277777777777778d0*tanh(125.0d0*r - 25.0d0) + &
      0.194444444444423d0)/r
S_A43=0
S_A44=-i*(ak/(0.1d0*tanh(0.10000000000000001d0*r - 0.10000000000000001d0) + &
      0.1d0*tanh(0.10000000000000001d0*r - 0.075000000000000011d0) + &
      0.1d0*tanh(0.10000000000000001d0*r - 0.050000000000000003d0) + &
      0.99250468320737d0) - 1.58113883008419d0*mC*sqrt(r*( &
      -0.0201510384166331d0*tanh(0.10000000000000001d0*r - &
      0.10000000000000001d0)**2 - 0.0201510384166331d0*tanh( &
      0.10000000000000001d0*r - 0.075000000000000011d0)**2 - &
      0.0201510384166331d0*tanh(0.10000000000000001d0*r - &
      0.050000000000000003d0)**2 + 0.0604531152498994d0)/( &
      0.100755192083166d0*tanh(0.10000000000000001d0*r - &
      0.10000000000000001d0) + 0.100755192083166d0*tanh( &
      0.10000000000000001d0*r - 0.075000000000000011d0) + &
      0.100755192083166d0*tanh(0.10000000000000001d0*r - &
      0.050000000000000003d0) + 1))/r)*(0.763888888888943d0*r + &
      3.56038411458294d0*(r - 0.2d0)**5 - 5.15407986111027d0*(r - 0.2d0 &
      )**4 + 0.651041666666159d0*(r - 0.2d0)**3 - 3.47222222222222d0* &
      tanh(125.0d0*r - 125.0d0)**2 - 3.47222222222222d0*tanh(125.0d0*r &
      - 110.71428571428572d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      96.428571428571431d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      82.142857142857139d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      67.857142857142861d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      53.571428571428584d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      39.285714285714292d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      25.0d0)**2 + 27.625d0)
S_B11=-gam*i*(0.00833333333333333d0*tanh(125.0d0*r - 125.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 100.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 75.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 50.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 25.0d0) + &
      0.166666666666667d0)*(0.763888888888943d0*r + 3.56038411458294d0* &
      (r - 0.2d0)**5 - 5.15407986111027d0*(r - 0.2d0)**4 + &
      0.651041666666159d0*(r - 0.2d0)**3 - 3.47222222222222d0*tanh( &
      125.0d0*r - 125.0d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      110.71428571428572d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      96.428571428571431d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      82.142857142857139d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      67.857142857142861d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      53.571428571428584d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      39.285714285714292d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      25.0d0)**2 + 27.625d0)
S_B12=0
S_B13=0
S_B14=0
S_B21=0
S_B22=-gam*i*(0.00833333333333333d0*tanh(125.0d0*r - 125.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 100.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 75.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 50.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 25.0d0) + &
      0.166666666666667d0)*(1.51909722222205d0*(r - 0.2d0)**3 - &
      1.82291666666646d0*(r - 0.2d0)**2 + 0.0277777777777778d0*tanh( &
      125.0d0*r - 125.0d0) + 0.0277777777777778d0*tanh(125.0d0*r - &
      110.71428571428572d0) + 0.0277777777777778d0*tanh(125.0d0*r - &
      96.428571428571431d0) + 0.0277777777777778d0*tanh(125.0d0*r - &
      82.142857142857139d0) + 0.0277777777777778d0*tanh(125.0d0*r - &
      67.857142857142861d0) + 0.0277777777777778d0*tanh(125.0d0*r - &
      53.571428571428584d0) + 0.0277777777777778d0*tanh(125.0d0*r - &
      39.285714285714292d0) + 0.0277777777777778d0*tanh(125.0d0*r - &
      25.0d0) + 0.194444444444423d0)
S_B23=0
S_B24=0
S_B31=0
S_B32=0
S_B33=-gam*i*(0.00833333333333333d0*tanh(125.0d0*r - 125.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 100.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 75.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 50.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 25.0d0) + &
      0.166666666666667d0)*(0.0454545454545455d0*tanh(2.5d0*r - 2.5d0) &
      + 0.0454545454545455d0*tanh(2.5d0*r - 2.2777777777777777d0) + &
      0.0454545454545455d0*tanh(2.5d0*r - 2.0555555555555554d0) + &
      0.0454545454545455d0*tanh(2.5d0*r - 1.8333333333333335d0) + &
      0.0454545454545455d0*tanh(2.5d0*r - 1.6111111111111112d0) + &
      0.0454545454545455d0*tanh(2.5d0*r - 1.3888888888888888d0) + &
      0.0454545454545455d0*tanh(2.5d0*r - 1.1666666666666667d0) + &
      0.0454545454545455d0*tanh(2.5d0*r - 0.94444444444444464d0) + &
      0.0454545454545455d0*tanh(2.5d0*r - 0.72222222222222232d0) + &
      0.0454545454545455d0*tanh(2.5d0*r - 0.5d0) + 0.707850846972511d0)
S_B34=-gam*i*one*(0.763888888888943d0*r + 3.56038411458294d0*(r - 0.2d0)**5 &
      - 5.15407986111027d0*(r - 0.2d0)**4 + 0.651041666666159d0*(r - &
      0.2d0)**3 - 3.47222222222222d0*tanh(125.0d0*r - 125.0d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 110.71428571428572d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 96.428571428571431d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 82.142857142857139d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 67.857142857142861d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 53.571428571428584d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 39.285714285714292d0)**2 - &
      3.47222222222222d0*tanh(125.0d0*r - 25.0d0)**2 + 27.625d0)
S_B41=0
S_B42=0
S_B43=-gam*i*one*(0.0454545454545455d0*tanh(2.5d0*r - 2.5d0) + &
      0.0454545454545455d0*tanh(2.5d0*r - 2.2777777777777777d0) + &
      0.0454545454545455d0*tanh(2.5d0*r - 2.0555555555555554d0) + &
      0.0454545454545455d0*tanh(2.5d0*r - 1.8333333333333335d0) + &
      0.0454545454545455d0*tanh(2.5d0*r - 1.6111111111111112d0) + &
      0.0454545454545455d0*tanh(2.5d0*r - 1.3888888888888888d0) + &
      0.0454545454545455d0*tanh(2.5d0*r - 1.1666666666666667d0) + &
      0.0454545454545455d0*tanh(2.5d0*r - 0.94444444444444464d0) + &
      0.0454545454545455d0*tanh(2.5d0*r - 0.72222222222222232d0) + &
      0.0454545454545455d0*tanh(2.5d0*r - 0.5d0) + 0.707850846972511d0)
S_B44=-gam*i*(0.00833333333333333d0*tanh(125.0d0*r - 125.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 100.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 75.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 50.0d0) + &
      0.00833333333333333d0*tanh(125.0d0*r - 25.0d0) + &
      0.166666666666667d0)*(0.763888888888943d0*r + 3.56038411458294d0* &
      (r - 0.2d0)**5 - 5.15407986111027d0*(r - 0.2d0)**4 + &
      0.651041666666159d0*(r - 0.2d0)**3 - 3.47222222222222d0*tanh( &
      125.0d0*r - 125.0d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      110.71428571428572d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      96.428571428571431d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      82.142857142857139d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      67.857142857142861d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      53.571428571428584d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      39.285714285714292d0)**2 - 3.47222222222222d0*tanh(125.0d0*r - &
      25.0d0)**2 + 27.625d0)


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
