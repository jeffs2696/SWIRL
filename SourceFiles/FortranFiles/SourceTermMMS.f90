 
    ! gam - axial wavenumber t
    ! ak  - reduced frequency
    ! kappa - ratio of specific heats
    ! i - imaginary number

    SUBROUTINE SourceCalc(& 
    r    , &
    S_1  , &
    S_2  , &
    S_3  , &
    S_4)

    REAL(KIND=rDef)   , DIMENSION(:), INTENT(IN) :: r
    COMPLEX(KIND=rDef), DIMENSION(:), INTENT(INOUT) :: S_1, S_2, S_3, S_4 

    ! Local variables
    COMPLEX(KIND=rDef) :: i   
    INTEGER :: jj, numberOfGridPoints 
    i = CMPLX(0.0, 1.0,KIND= rDef) 
    numberOfGridPoints = SIZE(r)

    DO jj = 1,numberOfGridPoints
    S_1(jj) = -i*(-1.0d0*0.5 + 1.0d0/1.0)*(-0.0*(-2.74348422496571d0*(r(jj) - 0.1d0)**3 + &
      3.7037037037037d0*(r(jj) - 0.1d0)**2) - 0.0*(2.74348422496571d0*(r(jj) - &
      0.1d0)**3 - 3.7037037037037d0*(r(jj) - 0.1d0)**2 + 1) + 0.0) + &
      6.66666666666667d0*r(jj) - 7.40740740740741d0*(r(jj) - 0.1d0)**2 - &
      0.666666666666667d0
S_2(jj) = 7.0d0*i*(-2.46913580246914d0*(r(jj) - 0.1d0)**3 + 3.33333333333333d0*(r(jj) - &
      0.1d0)**2 + 0.1d0)/r(jj)
S_3(jj) = 1.0d0*i*(-2.46913580246914d0*(r(jj) - 0.1d0)**3 + 3.33333333333333d0*(r(jj) - &
      0.1d0)**2 + 0.1d0)
S_4(jj) = -0.0*(-7.40740740740741d0*r(jj) + 8.23045267489712d0*(r(jj) - 0.1d0)**2 + &
      0.740740740740741d0) - 0.0*(7.40740740740741d0*r(jj) - &
      8.23045267489712d0*(r(jj) - 0.1d0)**2 - 0.740740740740741d0) - i*( &
      -1.0d0*0.5 + 1.0d0/1.0)*(-2.46913580246914d0*(r(jj) - 0.1d0)**3 + &
      3.33333333333333d0*(r(jj) - 0.1d0)**2 + 0.1d0) + 1.0d0*(-0.0*( &
      -2.74348422496571d0*(r(jj) - 0.1d0)**3 + 3.7037037037037d0*(r(jj) - 0.1d0 &
      )**2) - 0.0*(2.74348422496571d0*(r(jj) - 0.1d0)**3 - &
      3.7037037037037d0*(r(jj) - 0.1d0)**2 + 1) + 0.0)/r(jj)

    END DO
    
    IF (r(1).lt.10e-12) THEN
    S_1(1) = -i*(-1.0d0*0.5 + 1.0d0/1.0)*(-0.0*(-2.74348422496571d0*(r(1) - 0.1d0)**3 + &
      3.7037037037037d0*(r(1) - 0.1d0)**2) - 0.0*(2.74348422496571d0*(r(1) - &
      0.1d0)**3 - 3.7037037037037d0*(r(1) - 0.1d0)**2 + 1) + 0.0) + &
      6.66666666666667d0*r(1) - 7.40740740740741d0*(r(1) - 0.1d0)**2 - &
      0.666666666666667d0
S_2(1) = 0
S_3(1) = 1.0d0*i*(-2.46913580246914d0*(r(1) - 0.1d0)**3 + 3.33333333333333d0*(r(1) - &
      0.1d0)**2 + 0.1d0)
S_4(1) = 1.0d0*i*(-2.46913580246914d0*(r(1) - 0.1d0)**3 + 3.33333333333333d0*(r(1) - &
      0.1d0)**2 + 0.1d0)


        WRITE(0,*) 'rad is 0'
    ELSE
    ENDIF
    
    END SUBROUTINE SourceCalc
    