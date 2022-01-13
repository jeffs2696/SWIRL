 
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
S_A11=-i*(-1.58113883008419d0*mC*sqrt(r*(-0.0200398095526603d0*tanh(10.0d0*r - &
      10.0d0)**2 - 0.0200398095526603d0*tanh(10.0d0*r - 7.5d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 5.0d0)**2 + &
      0.0601194286579808d0)/(0.00100199047763301d0*tanh(10.0d0*r - &
      10.0d0) + 0.00100199047763301d0*tanh(10.0d0*r - 7.5d0) + &
      0.00100199047763301d0*tanh(10.0d0*r - 5.0d0) + 1))/r + 1d0/( &
      0.001d0*tanh(10.0d0*r - 10.0d0) + 0.001d0*tanh(10.0d0*r - 7.5d0) &
      + 0.001d0*tanh(10.0d0*r - 5.0d0) + 0.998013476497586d0))*( &
      6.27790178571429d0*(r - 0.2d0)**3 - 7.53348214285715d0*(r - 0.2d0 &
      )**2 + 0.0357142857142857d0*tanh(125.0d0*r - 125.0d0) + &
      0.0357142857142857d0*tanh(125.0d0*r - 108.99999999999999d0) + &
      0.0357142857142857d0*tanh(125.0d0*r - 93.0d0) + &
      0.0357142857142857d0*tanh(125.0d0*r - 77.0d0) + &
      0.0357142857142857d0*tanh(125.0d0*r - 61.0d0) + &
      0.0357142857142857d0*tanh(125.0d0*r - 45.0d0) + &
      1.42857142857143d0)
S_A12=-1.58113883008419d0*two*sqrt(r*(-0.0200398095526603d0*tanh(10.0d0*r - &
      10.0d0)**2 - 0.0200398095526603d0*tanh(10.0d0*r - 7.5d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 5.0d0)**2 + &
      0.0601194286579808d0)/(0.00100199047763301d0*tanh(10.0d0*r - &
      10.0d0) + 0.00100199047763301d0*tanh(10.0d0*r - 7.5d0) + &
      0.00100199047763301d0*tanh(10.0d0*r - 5.0d0) + 1))*( &
      0.0666666666666667d0*tanh(62.5d0*r - 62.5d0) + &
      0.0666666666666667d0*tanh(62.5d0*r - 22.5d0) + &
      0.933333333333333d0)/r
S_A13=0
S_A14=((2.44811642347632d0*r - 4.69911370018564d0*(r - 0.2d0)**2 - &
      1.45833333333333d0*tanh(12.5d0*r - 12.5d0)**2 - &
      1.45833333333333d0*tanh(12.5d0*r - 4.5d0)**2 + 2.45027440195381d0 &
      )/(0.0232310199824084d0*r - 1.56637123339521d0*(r - 0.2d0)**3 + &
      1.22405821173816d0*(r - 0.2d0)**2 + 0.116666666666667d0*tanh( &
      12.5d0*r - 12.5d0) + 0.116666666666667d0*tanh(12.5d0*r - 4.5d0) + &
      0.878687155595056d0) + (1.58113883008419d0*sqrt(r*( &
      -0.0200398095526603d0*tanh(10.0d0*r - 10.0d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 7.5d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 5.0d0)**2 + &
      0.0601194286579808d0)/(0.00100199047763301d0*tanh(10.0d0*r - &
      10.0d0) + 0.00100199047763301d0*tanh(10.0d0*r - 7.5d0) + &
      0.00100199047763301d0*tanh(10.0d0*r - 5.0d0) + 1)))**two*(1.4d0 - &
      one)/r)*(0.0232310199824084d0*r - 1.56637123339521d0*(r - 0.2d0) &
      **3 + 1.22405821173816d0*(r - 0.2d0)**2 + 0.116666666666667d0* &
      tanh(12.5d0*r - 12.5d0) + 0.116666666666667d0*tanh(12.5d0*r - &
      4.5d0) + 0.878687155595056d0)
S_A21=(1.58113883008419d0*sqrt(r*(-0.0200398095526603d0*tanh(10.0d0*r - 10.0d0 &
      )**2 - 0.0200398095526603d0*tanh(10.0d0*r - 7.5d0)**2 - &
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
      0.00100199047763301d0*tanh(10.0d0*r - 5.0d0) + 1)))**three*(1.4d0 &
      - one)/(r*two))*(6.27790178571429d0*(r - 0.2d0)**3 - &
      7.53348214285715d0*(r - 0.2d0)**2 + 0.0357142857142857d0*tanh( &
      125.0d0*r - 125.0d0) + 0.0357142857142857d0*tanh(125.0d0*r - &
      108.99999999999999d0) + 0.0357142857142857d0*tanh(125.0d0*r - &
      93.0d0) + 0.0357142857142857d0*tanh(125.0d0*r - 77.0d0) + &
      0.0357142857142857d0*tanh(125.0d0*r - 61.0d0) + &
      0.0357142857142857d0*tanh(125.0d0*r - 45.0d0) + &
      1.42857142857143d0)
S_A22=-i*(-1.58113883008419d0*mC*sqrt(r*(-0.0200398095526603d0*tanh(10.0d0*r - &
      10.0d0)**2 - 0.0200398095526603d0*tanh(10.0d0*r - 7.5d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 5.0d0)**2 + &
      0.0601194286579808d0)/(0.00100199047763301d0*tanh(10.0d0*r - &
      10.0d0) + 0.00100199047763301d0*tanh(10.0d0*r - 7.5d0) + &
      0.00100199047763301d0*tanh(10.0d0*r - 5.0d0) + 1))/r + 1d0/( &
      0.001d0*tanh(10.0d0*r - 10.0d0) + 0.001d0*tanh(10.0d0*r - 7.5d0) &
      + 0.001d0*tanh(10.0d0*r - 5.0d0) + 0.998013476497586d0))*( &
      0.0666666666666667d0*tanh(62.5d0*r - 62.5d0) + &
      0.0666666666666667d0*tanh(62.5d0*r - 22.5d0) + &
      0.933333333333333d0)
S_A23=0
S_A24=i*mC*(0.0232310199824084d0*r - 1.56637123339521d0*(r - 0.2d0)**3 + &
      1.22405821173816d0*(r - 0.2d0)**2 + 0.116666666666667d0*tanh( &
      12.5d0*r - 12.5d0) + 0.116666666666667d0*tanh(12.5d0*r - 4.5d0) + &
      0.878687155595056d0)/r
S_A31=(-1.25d0*tanh(125.0d0*r - 125.0d0)**2 - 1.25d0*tanh(125.0d0*r - &
      98.333333333333343d0)**2 - 1.25d0*tanh(125.0d0*r - &
      71.666666666666671d0)**2 - 1.25d0*tanh(125.0d0*r - 45.0d0)**2 + &
      5.0d0 + (1.58113883008419d0*sqrt(r*(-0.0200398095526603d0*tanh( &
      10.0d0*r - 10.0d0)**2 - 0.0200398095526603d0*tanh(10.0d0*r - &
      7.5d0)**2 - 0.0200398095526603d0*tanh(10.0d0*r - 5.0d0)**2 + &
      0.0601194286579808d0)/(0.00100199047763301d0*tanh(10.0d0*r - &
      10.0d0) + 0.00100199047763301d0*tanh(10.0d0*r - 7.5d0) + &
      0.00100199047763301d0*tanh(10.0d0*r - 5.0d0) + 1)))**two*(1.4d0 - &
      one)*(0.01d0*tanh(125.0d0*r - 125.0d0) + 0.01d0*tanh(125.0d0*r - &
      98.333333333333343d0) + 0.01d0*tanh(125.0d0*r - &
      71.666666666666671d0) + 0.01d0*tanh(125.0d0*r - 45.0d0) + 0.17d0) &
      /(r*two))*(6.27790178571429d0*(r - 0.2d0)**3 - 7.53348214285715d0 &
      *(r - 0.2d0)**2 + 0.0357142857142857d0*tanh(125.0d0*r - 125.0d0) &
      + 0.0357142857142857d0*tanh(125.0d0*r - 108.99999999999999d0) + &
      0.0357142857142857d0*tanh(125.0d0*r - 93.0d0) + &
      0.0357142857142857d0*tanh(125.0d0*r - 77.0d0) + &
      0.0357142857142857d0*tanh(125.0d0*r - 61.0d0) + &
      0.0357142857142857d0*tanh(125.0d0*r - 45.0d0) + &
      1.42857142857143d0)
S_A32=0
S_A33=-i*(-1.58113883008419d0*mC*sqrt(r*(-0.0200398095526603d0*tanh(10.0d0*r - &
      10.0d0)**2 - 0.0200398095526603d0*tanh(10.0d0*r - 7.5d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 5.0d0)**2 + &
      0.0601194286579808d0)/(0.00100199047763301d0*tanh(10.0d0*r - &
      10.0d0) + 0.00100199047763301d0*tanh(10.0d0*r - 7.5d0) + &
      0.00100199047763301d0*tanh(10.0d0*r - 5.0d0) + 1))/r + 1d0/( &
      0.001d0*tanh(10.0d0*r - 10.0d0) + 0.001d0*tanh(10.0d0*r - 7.5d0) &
      + 0.001d0*tanh(10.0d0*r - 5.0d0) + 0.998013476497586d0))*(0.1d0* &
      tanh(25.0d0*r - 25.0d0) + 0.1d0*tanh(25.0d0*r - &
      19.666666666666668d0) + 0.1d0*tanh(25.0d0*r - &
      14.333333333333334d0) + 0.1d0*tanh(25.0d0*r - 9.0d0) + &
      0.700004661820234d0)
S_A34=0
S_A41=(one/r + (-15.0669642857143d0*r + 18.8337053571429d0*(r - 0.2d0)**2 - &
      4.46428571428571d0*tanh(125.0d0*r - 125.0d0)**2 - &
      4.46428571428571d0*tanh(125.0d0*r - 108.99999999999999d0)**2 - &
      4.46428571428571d0*tanh(125.0d0*r - 93.0d0)**2 - &
      4.46428571428571d0*tanh(125.0d0*r - 77.0d0)**2 - &
      4.46428571428571d0*tanh(125.0d0*r - 61.0d0)**2 - &
      4.46428571428571d0*tanh(125.0d0*r - 45.0d0)**2 + &
      29.7991071428571d0)/(6.27790178571429d0*(r - 0.2d0)**3 - &
      7.53348214285715d0*(r - 0.2d0)**2 + 0.0357142857142857d0*tanh( &
      125.0d0*r - 125.0d0) + 0.0357142857142857d0*tanh(125.0d0*r - &
      108.99999999999999d0) + 0.0357142857142857d0*tanh(125.0d0*r - &
      93.0d0) + 0.0357142857142857d0*tanh(125.0d0*r - 77.0d0) + &
      0.0357142857142857d0*tanh(125.0d0*r - 61.0d0) + &
      0.0357142857142857d0*tanh(125.0d0*r - 45.0d0) + &
      1.42857142857143d0) + (1.58113883008419d0*sqrt(r*( &
      -0.0200398095526603d0*tanh(10.0d0*r - 10.0d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 7.5d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 5.0d0)**2 + &
      0.0601194286579808d0)/(0.00100199047763301d0*tanh(10.0d0*r - &
      10.0d0) + 0.00100199047763301d0*tanh(10.0d0*r - 7.5d0) + &
      0.00100199047763301d0*tanh(10.0d0*r - 5.0d0) + 1)))**two*(one + &
      1.4d0)/(r*two))*(6.27790178571429d0*(r - 0.2d0)**3 - &
      7.53348214285715d0*(r - 0.2d0)**2 + 0.0357142857142857d0*tanh( &
      125.0d0*r - 125.0d0) + 0.0357142857142857d0*tanh(125.0d0*r - &
      108.99999999999999d0) + 0.0357142857142857d0*tanh(125.0d0*r - &
      93.0d0) + 0.0357142857142857d0*tanh(125.0d0*r - 77.0d0) + &
      0.0357142857142857d0*tanh(125.0d0*r - 61.0d0) + &
      0.0357142857142857d0*tanh(125.0d0*r - 45.0d0) + &
      1.42857142857143d0)
S_A42=i*mC*(0.0666666666666667d0*tanh(62.5d0*r - 62.5d0) + 0.0666666666666667d0 &
      *tanh(62.5d0*r - 22.5d0) + 0.933333333333333d0)/r
S_A43=0
S_A44=-i*(-1.58113883008419d0*mC*sqrt(r*(-0.0200398095526603d0*tanh(10.0d0*r - &
      10.0d0)**2 - 0.0200398095526603d0*tanh(10.0d0*r - 7.5d0)**2 - &
      0.0200398095526603d0*tanh(10.0d0*r - 5.0d0)**2 + &
      0.0601194286579808d0)/(0.00100199047763301d0*tanh(10.0d0*r - &
      10.0d0) + 0.00100199047763301d0*tanh(10.0d0*r - 7.5d0) + &
      0.00100199047763301d0*tanh(10.0d0*r - 5.0d0) + 1))/r + 1d0/( &
      0.001d0*tanh(10.0d0*r - 10.0d0) + 0.001d0*tanh(10.0d0*r - 7.5d0) &
      + 0.001d0*tanh(10.0d0*r - 5.0d0) + 0.998013476497586d0))*( &
      0.0232310199824084d0*r - 1.56637123339521d0*(r - 0.2d0)**3 + &
      1.22405821173816d0*(r - 0.2d0)**2 + 0.116666666666667d0*tanh( &
      12.5d0*r - 12.5d0) + 0.116666666666667d0*tanh(12.5d0*r - 4.5d0) + &
      0.878687155595056d0)
S_B11=-i*(0.01d0*tanh(125.0d0*r - 125.0d0) + 0.01d0*tanh(125.0d0*r - &
      98.333333333333343d0) + 0.01d0*tanh(125.0d0*r - &
      71.666666666666671d0) + 0.01d0*tanh(125.0d0*r - 45.0d0) + 0.17d0) &
      *(6.27790178571429d0*(r - 0.2d0)**3 - 7.53348214285715d0*(r - &
      0.2d0)**2 + 0.0357142857142857d0*tanh(125.0d0*r - 125.0d0) + &
      0.0357142857142857d0*tanh(125.0d0*r - 108.99999999999999d0) + &
      0.0357142857142857d0*tanh(125.0d0*r - 93.0d0) + &
      0.0357142857142857d0*tanh(125.0d0*r - 77.0d0) + &
      0.0357142857142857d0*tanh(125.0d0*r - 61.0d0) + &
      0.0357142857142857d0*tanh(125.0d0*r - 45.0d0) + &
      1.42857142857143d0)
S_B12=0
S_B13=0
S_B14=0
S_B21=0
S_B22=-i*(0.0666666666666667d0*tanh(62.5d0*r - 62.5d0) + 0.0666666666666667d0* &
      tanh(62.5d0*r - 22.5d0) + 0.933333333333333d0)*(0.01d0*tanh( &
      125.0d0*r - 125.0d0) + 0.01d0*tanh(125.0d0*r - &
      98.333333333333343d0) + 0.01d0*tanh(125.0d0*r - &
      71.666666666666671d0) + 0.01d0*tanh(125.0d0*r - 45.0d0) + 0.17d0)
S_B23=0
S_B24=0
S_B31=0
S_B32=0
S_B33=-i*(0.1d0*tanh(25.0d0*r - 25.0d0) + 0.1d0*tanh(25.0d0*r - &
      19.666666666666668d0) + 0.1d0*tanh(25.0d0*r - &
      14.333333333333334d0) + 0.1d0*tanh(25.0d0*r - 9.0d0) + &
      0.700004661820234d0)*(0.01d0*tanh(125.0d0*r - 125.0d0) + 0.01d0* &
      tanh(125.0d0*r - 98.333333333333343d0) + 0.01d0*tanh(125.0d0*r - &
      71.666666666666671d0) + 0.01d0*tanh(125.0d0*r - 45.0d0) + 0.17d0)
S_B34=-i*one*(0.0232310199824084d0*r - 1.56637123339521d0*(r - 0.2d0)**3 + &
      1.22405821173816d0*(r - 0.2d0)**2 + 0.116666666666667d0*tanh( &
      12.5d0*r - 12.5d0) + 0.116666666666667d0*tanh(12.5d0*r - 4.5d0) + &
      0.878687155595056d0)
S_B41=0
S_B42=0
S_B43=-i*one*(0.1d0*tanh(25.0d0*r - 25.0d0) + 0.1d0*tanh(25.0d0*r - &
      19.666666666666668d0) + 0.1d0*tanh(25.0d0*r - &
      14.333333333333334d0) + 0.1d0*tanh(25.0d0*r - 9.0d0) + &
      0.700004661820234d0)
S_B44=-i*(0.01d0*tanh(125.0d0*r - 125.0d0) + 0.01d0*tanh(125.0d0*r - &
      98.333333333333343d0) + 0.01d0*tanh(125.0d0*r - &
      71.666666666666671d0) + 0.01d0*tanh(125.0d0*r - 45.0d0) + 0.17d0) &
      *(0.0232310199824084d0*r - 1.56637123339521d0*(r - 0.2d0)**3 + &
      1.22405821173816d0*(r - 0.2d0)**2 + 0.116666666666667d0*tanh( &
      12.5d0*r - 12.5d0) + 0.116666666666667d0*tanh(12.5d0*r - 4.5d0) + &
      0.878687155595056d0)



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
