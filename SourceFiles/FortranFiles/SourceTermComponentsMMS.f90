 
! gam - axial wavenumber t
! ak  - reduced frequency
! kappa - ratio of specific heats
! i - imaginary number
 
    SUBROUTINE SourceCalcComponents(& 
    gam  , &
    i    , &
    ak   , &
    kappa, &
    m    , & 
    r    , &
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
    REAL(KIND=rDef)   , INTENT(IN) :: kappa,r,r_max
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
    

    
    ! Local variables
    COMPLEX(KIND=rDef) :: mC, kappaC, rC, r_maxC
    
    REAL(KIND=rDef) :: one,two,three

    one = REAL(1.0,KIND=rDef)    
    two = REAL(2.0,KIND=rDef)    
    three = REAL(3.0,KIND=rDef)

    mC = CMPLX(m,KIND=rDef)
    kappaC = CMPLX(kappa,KIND=rDef)
    rC = CMPLX(r,KIND=rDef)
    r_maxC = CMPLX(r_max,KIND=rDef)
S_A11=-i*(-1.58113883008419d0*mC*sqrt(r*1d0/(0.187658336365052d0*tanh( &
      0.0037499999999999999d0*r - 0.0037499999999999999d0) + &
      0.187658336365052d0*tanh(0.0037499999999999999d0*r - &
      0.0022500000000000003d0) + 0.187658336365052d0*tanh( &
      0.0037499999999999999d0*r - 0.00075000000000000002d0) + 1)*( &
      -0.00140743752273789d0*tanh(0.0037499999999999999d0*r - &
      0.0037499999999999999d0)**2 - 0.00140743752273789d0*tanh( &
      0.0037499999999999999d0*r - 0.0022500000000000003d0)**2 - &
      0.00140743752273789d0*tanh(0.0037499999999999999d0*r - &
      0.00075000000000000002d0)**2 + 0.00422231256821366d0))/r + 10.0d0 &
      /(0.1875d0*tanh(0.0037499999999999999d0*r - &
      0.0037499999999999999d0) + 0.1875d0*tanh(0.0037499999999999999d0* &
      r - 0.0022500000000000003d0) + 0.1875d0*tanh( &
      0.0037499999999999999d0*r - 0.00075000000000000002d0) + &
      0.999156251898431d0))*(2.05447160938161d0*(r - 0.2d0)**3 - &
      2.46536593125793d0*(r - 0.2d0)**2 + 0.125d0*tanh(1.25d0*r - &
      1.25d0) + 0.125d0*tanh(1.25d0*r - 1.0d0) + 0.125d0*tanh(1.25d0*r &
      - 0.75d0) + 0.125d0*tanh(1.25d0*r - 0.5d0) + 0.125d0*tanh(1.25d0* &
      r - 0.25d0) + 0.262972366000846d0)
S_A12=-3.16227766016838d0*sqrt(r*1d0/(0.187658336365052d0*tanh( &
      0.0037499999999999999d0*r - 0.0037499999999999999d0) + &
      0.187658336365052d0*tanh(0.0037499999999999999d0*r - &
      0.0022500000000000003d0) + 0.187658336365052d0*tanh( &
      0.0037499999999999999d0*r - 0.00075000000000000002d0) + 1)*( &
      -0.00140743752273789d0*tanh(0.0037499999999999999d0*r - &
      0.0037499999999999999d0)**2 - 0.00140743752273789d0*tanh( &
      0.0037499999999999999d0*r - 0.0022500000000000003d0)**2 - &
      0.00140743752273789d0*tanh(0.0037499999999999999d0*r - &
      0.00075000000000000002d0)**2 + 0.00422231256821366d0))*(0.125d0* &
      tanh(25.0d0*r - 25.0d0) + 0.125d0*tanh(25.0d0*r - 20.0d0) + &
      0.125d0*tanh(25.0d0*r - 15.0d0) + 0.125d0*tanh(25.0d0*r - 10.0d0 &
      ) + 0.125d0*tanh(25.0d0*r - 5.0d0) + 0.500011349982487d0)/r
S_A13=0
S_A14=(1.0d0*(11.5295969398069d0*r - 14.0976514193326d0*(r - 0.2d0)**2 - &
      1.5625d0*tanh(12.5d0*r - 12.5d0)**2 - 1.5625d0*tanh(12.5d0*r - &
      10.0d0)**2 - 1.5625d0*tanh(12.5d0*r - 7.5d0)**2 - 1.5625d0*tanh( &
      12.5d0*r - 5.0d0)**2 - 1.5625d0*tanh(12.5d0*r - 2.5d0)**2 + &
      3.90224080557348d0)/(-1.60433980646514d0*r - 4.69921713977753d0*( &
      r - 0.2d0)**3 + 5.76479846990345d0*(r - 0.2d0)**2 + 0.125d0*tanh( &
      12.5d0*r - 12.5d0) + 0.125d0*tanh(12.5d0*r - 10.0d0) + 0.125d0* &
      tanh(12.5d0*r - 7.5d0) + 0.125d0*tanh(12.5d0*r - 5.0d0) + 0.125d0 &
      *tanh(12.5d0*r - 2.5d0) + 0.82255260048212d0) + 1.0d0*(r*1d0/( &
      0.187658336365052d0*tanh(0.0037499999999999999d0*r - &
      0.0037499999999999999d0) + 0.187658336365052d0*tanh( &
      0.0037499999999999999d0*r - 0.0022500000000000003d0) + &
      0.187658336365052d0*tanh(0.0037499999999999999d0*r - &
      0.00075000000000000002d0) + 1)*(-0.00140743752273789d0*tanh( &
      0.0037499999999999999d0*r - 0.0037499999999999999d0)**2 - &
      0.00140743752273789d0*tanh(0.0037499999999999999d0*r - &
      0.0022500000000000003d0)**2 - 0.00140743752273789d0*tanh( &
      0.0037499999999999999d0*r - 0.00075000000000000002d0)**2 + &
      0.00422231256821366d0))**1.0d0/r)*(-1.60433980646514d0*r - &
      4.69921713977753d0*(r - 0.2d0)**3 + 5.76479846990345d0*(r - 0.2d0 &
      )**2 + 0.125d0*tanh(12.5d0*r - 12.5d0) + 0.125d0*tanh(12.5d0*r - &
      10.0d0) + 0.125d0*tanh(12.5d0*r - 7.5d0) + 0.125d0*tanh(12.5d0*r &
      - 5.0d0) + 0.125d0*tanh(12.5d0*r - 2.5d0) + 0.82255260048212d0)
S_A21=(1.58113883008419d0*sqrt(r*1d0/(0.187658336365052d0*tanh( &
      0.0037499999999999999d0*r - 0.0037499999999999999d0) + &
      0.187658336365052d0*tanh(0.0037499999999999999d0*r - &
      0.0022500000000000003d0) + 0.187658336365052d0*tanh( &
      0.0037499999999999999d0*r - 0.00075000000000000002d0) + 1)*( &
      -0.00140743752273789d0*tanh(0.0037499999999999999d0*r - &
      0.0037499999999999999d0)**2 - 0.00140743752273789d0*tanh( &
      0.0037499999999999999d0*r - 0.0022500000000000003d0)**2 - &
      0.00140743752273789d0*tanh(0.0037499999999999999d0*r - &
      0.00075000000000000002d0)**2 + 0.00422231256821366d0))*(0.5d0*r*( &
      -0.00140743752273789d0*(0.0075d0 - 0.0075d0*tanh( &
      0.0037499999999999999d0*r - 0.0037499999999999999d0)**2)*tanh( &
      0.0037499999999999999d0*r - 0.0037499999999999999d0) - &
      0.00140743752273789d0*(0.0075d0 - 0.0075d0*tanh( &
      0.0037499999999999999d0*r - 0.0022500000000000003d0)**2)*tanh( &
      0.0037499999999999999d0*r - 0.0022500000000000003d0) - &
      0.00140743752273789d0*(0.0075d0 - 0.0075d0*tanh( &
      0.0037499999999999999d0*r - 0.00075000000000000002d0)**2)*tanh( &
      0.0037499999999999999d0*r - 0.00075000000000000002d0))*1d0/( &
      0.187658336365052d0*tanh(0.0037499999999999999d0*r - &
      0.0037499999999999999d0) + 0.187658336365052d0*tanh( &
      0.0037499999999999999d0*r - 0.0022500000000000003d0) + &
      0.187658336365052d0*tanh(0.0037499999999999999d0*r - &
      0.00075000000000000002d0) + 1) + 0.5d0*r*(0.187658336365052d0* &
      tanh(0.0037499999999999999d0*r - 0.0037499999999999999d0) + &
      0.187658336365052d0*tanh(0.0037499999999999999d0*r - &
      0.0022500000000000003d0) + 0.187658336365052d0*tanh( &
      0.0037499999999999999d0*r - 0.00075000000000000002d0) + 1)**( &
      -2.0d0)*(-0.00140743752273789d0*tanh(0.0037499999999999999d0*r - &
      0.0037499999999999999d0)**2 - 0.00140743752273789d0*tanh( &
      0.0037499999999999999d0*r - 0.0022500000000000003d0)**2 - &
      0.00140743752273789d0*tanh(0.0037499999999999999d0*r - &
      0.00075000000000000002d0)**2 + 0.00422231256821366d0)*( &
      0.000703718761368943d0*tanh(0.0037499999999999999d0*r - &
      0.0037499999999999999d0)**2 + 0.000703718761368943d0*tanh( &
      0.0037499999999999999d0*r - 0.0022500000000000003d0)**2 + &
      0.000703718761368943d0*tanh(0.0037499999999999999d0*r - &
      0.00075000000000000002d0)**2 - 0.00211115628410683d0) + 0.5d0*1d0 &
      /(0.187658336365052d0*tanh(0.0037499999999999999d0*r - &
      0.0037499999999999999d0) + 0.187658336365052d0*tanh( &
      0.0037499999999999999d0*r - 0.0022500000000000003d0) + &
      0.187658336365052d0*tanh(0.0037499999999999999d0*r - &
      0.00075000000000000002d0) + 1)*(-0.00140743752273789d0*tanh( &
      0.0037499999999999999d0*r - 0.0037499999999999999d0)**2 - &
      0.00140743752273789d0*tanh(0.0037499999999999999d0*r - &
      0.0022500000000000003d0)**2 - 0.00140743752273789d0*tanh( &
      0.0037499999999999999d0*r - 0.00075000000000000002d0)**2 + &
      0.00422231256821366d0))*(0.187658336365052d0*tanh( &
      0.0037499999999999999d0*r - 0.0037499999999999999d0) + &
      0.187658336365052d0*tanh(0.0037499999999999999d0*r - &
      0.0022500000000000003d0) + 0.187658336365052d0*tanh( &
      0.0037499999999999999d0*r - 0.00075000000000000002d0) + 1)**1.0d0 &
      /(r*(-0.00140743752273789d0*tanh(0.0037499999999999999d0*r - &
      0.0037499999999999999d0)**2 - 0.00140743752273789d0*tanh( &
      0.0037499999999999999d0*r - 0.0022500000000000003d0)**2 - &
      0.00140743752273789d0*tanh(0.0037499999999999999d0*r - &
      0.00075000000000000002d0)**2 + 0.00422231256821366d0)) + &
      1.58113883008419d0*sqrt(r*1d0/(0.187658336365052d0*tanh( &
      0.0037499999999999999d0*r - 0.0037499999999999999d0) + &
      0.187658336365052d0*tanh(0.0037499999999999999d0*r - &
      0.0022500000000000003d0) + 0.187658336365052d0*tanh( &
      0.0037499999999999999d0*r - 0.00075000000000000002d0) + 1)*( &
      -0.00140743752273789d0*tanh(0.0037499999999999999d0*r - &
      0.0037499999999999999d0)**2 - 0.00140743752273789d0*tanh( &
      0.0037499999999999999d0*r - 0.0022500000000000003d0)**2 - &
      0.00140743752273789d0*tanh(0.0037499999999999999d0*r - &
      0.00075000000000000002d0)**2 + 0.00422231256821366d0))/r + &
      0.790569415042095d0*(r*1d0/(0.187658336365052d0*tanh( &
      0.0037499999999999999d0*r - 0.0037499999999999999d0) + &
      0.187658336365052d0*tanh(0.0037499999999999999d0*r - &
      0.0022500000000000003d0) + 0.187658336365052d0*tanh( &
      0.0037499999999999999d0*r - 0.00075000000000000002d0) + 1)*( &
      -0.00140743752273789d0*tanh(0.0037499999999999999d0*r - &
      0.0037499999999999999d0)**2 - 0.00140743752273789d0*tanh( &
      0.0037499999999999999d0*r - 0.0022500000000000003d0)**2 - &
      0.00140743752273789d0*tanh(0.0037499999999999999d0*r - &
      0.00075000000000000002d0)**2 + 0.00422231256821366d0))**1.5d0/r)* &
      (2.05447160938161d0*(r - 0.2d0)**3 - 2.46536593125793d0*(r - &
      0.2d0)**2 + 0.125d0*tanh(1.25d0*r - 1.25d0) + 0.125d0*tanh(1.25d0 &
      *r - 1.0d0) + 0.125d0*tanh(1.25d0*r - 0.75d0) + 0.125d0*tanh( &
      1.25d0*r - 0.5d0) + 0.125d0*tanh(1.25d0*r - 0.25d0) + &
      0.262972366000846d0)
S_A22=-i*(-1.58113883008419d0*mC*sqrt(r*1d0/(0.187658336365052d0*tanh( &
      0.0037499999999999999d0*r - 0.0037499999999999999d0) + &
      0.187658336365052d0*tanh(0.0037499999999999999d0*r - &
      0.0022500000000000003d0) + 0.187658336365052d0*tanh( &
      0.0037499999999999999d0*r - 0.00075000000000000002d0) + 1)*( &
      -0.00140743752273789d0*tanh(0.0037499999999999999d0*r - &
      0.0037499999999999999d0)**2 - 0.00140743752273789d0*tanh( &
      0.0037499999999999999d0*r - 0.0022500000000000003d0)**2 - &
      0.00140743752273789d0*tanh(0.0037499999999999999d0*r - &
      0.00075000000000000002d0)**2 + 0.00422231256821366d0))/r + 10.0d0 &
      /(0.1875d0*tanh(0.0037499999999999999d0*r - &
      0.0037499999999999999d0) + 0.1875d0*tanh(0.0037499999999999999d0* &
      r - 0.0022500000000000003d0) + 0.1875d0*tanh( &
      0.0037499999999999999d0*r - 0.00075000000000000002d0) + &
      0.999156251898431d0))*(0.125d0*tanh(25.0d0*r - 25.0d0) + 0.125d0* &
      tanh(25.0d0*r - 20.0d0) + 0.125d0*tanh(25.0d0*r - 15.0d0) + &
      0.125d0*tanh(25.0d0*r - 10.0d0) + 0.125d0*tanh(25.0d0*r - 5.0d0) &
      + 0.500011349982487d0)
S_A23=0
S_A24=i*mC*(-1.60433980646514d0*r - 4.69921713977753d0*(r - 0.2d0)**3 + &
      5.76479846990345d0*(r - 0.2d0)**2 + 0.125d0*tanh(12.5d0*r - &
      12.5d0) + 0.125d0*tanh(12.5d0*r - 10.0d0) + 0.125d0*tanh(12.5d0*r &
      - 7.5d0) + 0.125d0*tanh(12.5d0*r - 5.0d0) + 0.125d0*tanh(12.5d0*r &
      - 2.5d0) + 0.82255260048212d0)/r
S_A31=(-0.78125d0*tanh(62.5d0*r - 62.5d0)**2 - 0.78125d0*tanh(62.5d0*r - &
      50.0d0)**2 - 0.78125d0*tanh(62.5d0*r - 37.5d0)**2 - 0.78125d0* &
      tanh(62.5d0*r - 25.0d0)**2 - 0.78125d0*tanh(62.5d0*r - 12.5d0)**2 &
      + 3.90625d0 + 0.5d0*(r*1d0/(0.187658336365052d0*tanh( &
      0.0037499999999999999d0*r - 0.0037499999999999999d0) + &
      0.187658336365052d0*tanh(0.0037499999999999999d0*r - &
      0.0022500000000000003d0) + 0.187658336365052d0*tanh( &
      0.0037499999999999999d0*r - 0.00075000000000000002d0) + 1)*( &
      -0.00140743752273789d0*tanh(0.0037499999999999999d0*r - &
      0.0037499999999999999d0)**2 - 0.00140743752273789d0*tanh( &
      0.0037499999999999999d0*r - 0.0022500000000000003d0)**2 - &
      0.00140743752273789d0*tanh(0.0037499999999999999d0*r - &
      0.00075000000000000002d0)**2 + 0.00422231256821366d0))**1.0d0*( &
      0.0125d0*tanh(62.5d0*r - 62.5d0) + 0.0125d0*tanh(62.5d0*r - &
      50.0d0) + 0.0125d0*tanh(62.5d0*r - 37.5d0) + 0.0125d0*tanh(62.5d0 &
      *r - 25.0d0) + 0.0125d0*tanh(62.5d0*r - 12.5d0) + &
      0.0500000000003472d0)/r)*(2.05447160938161d0*(r - 0.2d0)**3 - &
      2.46536593125793d0*(r - 0.2d0)**2 + 0.125d0*tanh(1.25d0*r - &
      1.25d0) + 0.125d0*tanh(1.25d0*r - 1.0d0) + 0.125d0*tanh(1.25d0*r &
      - 0.75d0) + 0.125d0*tanh(1.25d0*r - 0.5d0) + 0.125d0*tanh(1.25d0* &
      r - 0.25d0) + 0.262972366000846d0)
S_A32=0
S_A33=-i*(-1.58113883008419d0*mC*sqrt(r*1d0/(0.187658336365052d0*tanh( &
      0.0037499999999999999d0*r - 0.0037499999999999999d0) + &
      0.187658336365052d0*tanh(0.0037499999999999999d0*r - &
      0.0022500000000000003d0) + 0.187658336365052d0*tanh( &
      0.0037499999999999999d0*r - 0.00075000000000000002d0) + 1)*( &
      -0.00140743752273789d0*tanh(0.0037499999999999999d0*r - &
      0.0037499999999999999d0)**2 - 0.00140743752273789d0*tanh( &
      0.0037499999999999999d0*r - 0.0022500000000000003d0)**2 - &
      0.00140743752273789d0*tanh(0.0037499999999999999d0*r - &
      0.00075000000000000002d0)**2 + 0.00422231256821366d0))/r + 10.0d0 &
      /(0.1875d0*tanh(0.0037499999999999999d0*r - &
      0.0037499999999999999d0) + 0.1875d0*tanh(0.0037499999999999999d0* &
      r - 0.0022500000000000003d0) + 0.1875d0*tanh( &
      0.0037499999999999999d0*r - 0.00075000000000000002d0) + &
      0.999156251898431d0))*(0.125d0*tanh(25.0d0*r - 25.0d0) + 0.125d0* &
      tanh(25.0d0*r - 20.0d0) + 0.125d0*tanh(25.0d0*r - 15.0d0) + &
      0.125d0*tanh(25.0d0*r - 10.0d0) + 0.125d0*tanh(25.0d0*r - 5.0d0) &
      + 0.500011349982487d0)
S_A34=0
S_A41=(1.0d0*(-4.93073186251587d0*r + 6.16341482814484d0*(r - 0.2d0)**2 - &
      0.15625d0*tanh(1.25d0*r - 1.25d0)**2 - 0.15625d0*tanh(1.25d0*r - &
      1.0d0)**2 - 0.15625d0*tanh(1.25d0*r - 0.75d0)**2 - 0.15625d0*tanh &
      (1.25d0*r - 0.5d0)**2 - 0.15625d0*tanh(1.25d0*r - 0.25d0)**2 + &
      1.76739637250317d0)/(2.05447160938161d0*(r - 0.2d0)**3 - &
      2.46536593125793d0*(r - 0.2d0)**2 + 0.125d0*tanh(1.25d0*r - &
      1.25d0) + 0.125d0*tanh(1.25d0*r - 1.0d0) + 0.125d0*tanh(1.25d0*r &
      - 0.75d0) + 0.125d0*tanh(1.25d0*r - 0.5d0) + 0.125d0*tanh(1.25d0* &
      r - 0.25d0) + 0.262972366000846d0) + 3.0d0*(r*1d0/( &
      0.187658336365052d0*tanh(0.0037499999999999999d0*r - &
      0.0037499999999999999d0) + 0.187658336365052d0*tanh( &
      0.0037499999999999999d0*r - 0.0022500000000000003d0) + &
      0.187658336365052d0*tanh(0.0037499999999999999d0*r - &
      0.00075000000000000002d0) + 1)*(-0.00140743752273789d0*tanh( &
      0.0037499999999999999d0*r - 0.0037499999999999999d0)**2 - &
      0.00140743752273789d0*tanh(0.0037499999999999999d0*r - &
      0.0022500000000000003d0)**2 - 0.00140743752273789d0*tanh( &
      0.0037499999999999999d0*r - 0.00075000000000000002d0)**2 + &
      0.00422231256821366d0))**1.0d0/r + 1.0d0/r)*(2.05447160938161d0*( &
      r - 0.2d0)**3 - 2.46536593125793d0*(r - 0.2d0)**2 + 0.125d0*tanh( &
      1.25d0*r - 1.25d0) + 0.125d0*tanh(1.25d0*r - 1.0d0) + 0.125d0* &
      tanh(1.25d0*r - 0.75d0) + 0.125d0*tanh(1.25d0*r - 0.5d0) + &
      0.125d0*tanh(1.25d0*r - 0.25d0) + 0.262972366000846d0)
S_A42=i*mC*(0.125d0*tanh(25.0d0*r - 25.0d0) + 0.125d0*tanh(25.0d0*r - 20.0d0) + &
      0.125d0*tanh(25.0d0*r - 15.0d0) + 0.125d0*tanh(25.0d0*r - 10.0d0 &
      ) + 0.125d0*tanh(25.0d0*r - 5.0d0) + 0.500011349982487d0)/r
S_A43=0
S_A44=-i*(-1.58113883008419d0*mC*sqrt(r*1d0/(0.187658336365052d0*tanh( &
      0.0037499999999999999d0*r - 0.0037499999999999999d0) + &
      0.187658336365052d0*tanh(0.0037499999999999999d0*r - &
      0.0022500000000000003d0) + 0.187658336365052d0*tanh( &
      0.0037499999999999999d0*r - 0.00075000000000000002d0) + 1)*( &
      -0.00140743752273789d0*tanh(0.0037499999999999999d0*r - &
      0.0037499999999999999d0)**2 - 0.00140743752273789d0*tanh( &
      0.0037499999999999999d0*r - 0.0022500000000000003d0)**2 - &
      0.00140743752273789d0*tanh(0.0037499999999999999d0*r - &
      0.00075000000000000002d0)**2 + 0.00422231256821366d0))/r + 10.0d0 &
      /(0.1875d0*tanh(0.0037499999999999999d0*r - &
      0.0037499999999999999d0) + 0.1875d0*tanh(0.0037499999999999999d0* &
      r - 0.0022500000000000003d0) + 0.1875d0*tanh( &
      0.0037499999999999999d0*r - 0.00075000000000000002d0) + &
      0.999156251898431d0))*(-1.60433980646514d0*r - 4.69921713977753d0 &
      *(r - 0.2d0)**3 + 5.76479846990345d0*(r - 0.2d0)**2 + 0.125d0* &
      tanh(12.5d0*r - 12.5d0) + 0.125d0*tanh(12.5d0*r - 10.0d0) + &
      0.125d0*tanh(12.5d0*r - 7.5d0) + 0.125d0*tanh(12.5d0*r - 5.0d0) + &
      0.125d0*tanh(12.5d0*r - 2.5d0) + 0.82255260048212d0)
S_B11=-10.0d0*i*(0.0125d0*tanh(62.5d0*r - 62.5d0) + 0.0125d0*tanh(62.5d0*r - &
      50.0d0) + 0.0125d0*tanh(62.5d0*r - 37.5d0) + 0.0125d0*tanh(62.5d0 &
      *r - 25.0d0) + 0.0125d0*tanh(62.5d0*r - 12.5d0) + &
      0.0500000000003472d0)*(2.05447160938161d0*(r - 0.2d0)**3 - &
      2.46536593125793d0*(r - 0.2d0)**2 + 0.125d0*tanh(1.25d0*r - &
      1.25d0) + 0.125d0*tanh(1.25d0*r - 1.0d0) + 0.125d0*tanh(1.25d0*r &
      - 0.75d0) + 0.125d0*tanh(1.25d0*r - 0.5d0) + 0.125d0*tanh(1.25d0* &
      r - 0.25d0) + 0.262972366000846d0)
S_B12=0
S_B13=0
S_B14=0
S_B21=0
S_B22=-10.0d0*i*(0.125d0*tanh(25.0d0*r - 25.0d0) + 0.125d0*tanh(25.0d0*r - &
      20.0d0) + 0.125d0*tanh(25.0d0*r - 15.0d0) + 0.125d0*tanh(25.0d0*r &
      - 10.0d0) + 0.125d0*tanh(25.0d0*r - 5.0d0) + 0.500011349982487d0) &
      *(0.0125d0*tanh(62.5d0*r - 62.5d0) + 0.0125d0*tanh(62.5d0*r - &
      50.0d0) + 0.0125d0*tanh(62.5d0*r - 37.5d0) + 0.0125d0*tanh(62.5d0 &
      *r - 25.0d0) + 0.0125d0*tanh(62.5d0*r - 12.5d0) + &
      0.0500000000003472d0)
S_B23=0
S_B24=0
S_B31=0
S_B32=0
S_B33=-10.0d0*i*(0.125d0*tanh(25.0d0*r - 25.0d0) + 0.125d0*tanh(25.0d0*r - &
      20.0d0) + 0.125d0*tanh(25.0d0*r - 15.0d0) + 0.125d0*tanh(25.0d0*r &
      - 10.0d0) + 0.125d0*tanh(25.0d0*r - 5.0d0) + 0.500011349982487d0) &
      *(0.0125d0*tanh(62.5d0*r - 62.5d0) + 0.0125d0*tanh(62.5d0*r - &
      50.0d0) + 0.0125d0*tanh(62.5d0*r - 37.5d0) + 0.0125d0*tanh(62.5d0 &
      *r - 25.0d0) + 0.0125d0*tanh(62.5d0*r - 12.5d0) + &
      0.0500000000003472d0)
S_B34=-10.0d0*i*(-1.60433980646514d0*r - 4.69921713977753d0*(r - 0.2d0)**3 + &
      5.76479846990345d0*(r - 0.2d0)**2 + 0.125d0*tanh(12.5d0*r - &
      12.5d0) + 0.125d0*tanh(12.5d0*r - 10.0d0) + 0.125d0*tanh(12.5d0*r &
      - 7.5d0) + 0.125d0*tanh(12.5d0*r - 5.0d0) + 0.125d0*tanh(12.5d0*r &
      - 2.5d0) + 0.82255260048212d0)
S_B41=0
S_B42=0
S_B43=-10.0d0*i*(0.125d0*tanh(25.0d0*r - 25.0d0) + 0.125d0*tanh(25.0d0*r - &
      20.0d0) + 0.125d0*tanh(25.0d0*r - 15.0d0) + 0.125d0*tanh(25.0d0*r &
      - 10.0d0) + 0.125d0*tanh(25.0d0*r - 5.0d0) + 0.500011349982487d0)
S_B44=-10.0d0*i*(0.0125d0*tanh(62.5d0*r - 62.5d0) + 0.0125d0*tanh(62.5d0*r - &
      50.0d0) + 0.0125d0*tanh(62.5d0*r - 37.5d0) + 0.0125d0*tanh(62.5d0 &
      *r - 25.0d0) + 0.0125d0*tanh(62.5d0*r - 12.5d0) + &
      0.0500000000003472d0)*(-1.60433980646514d0*r - 4.69921713977753d0 &
      *(r - 0.2d0)**3 + 5.76479846990345d0*(r - 0.2d0)**2 + 0.125d0* &
      tanh(12.5d0*r - 12.5d0) + 0.125d0*tanh(12.5d0*r - 10.0d0) + &
      0.125d0*tanh(12.5d0*r - 7.5d0) + 0.125d0*tanh(12.5d0*r - 5.0d0) + &
      0.125d0*tanh(12.5d0*r - 2.5d0) + 0.82255260048212d0)



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
