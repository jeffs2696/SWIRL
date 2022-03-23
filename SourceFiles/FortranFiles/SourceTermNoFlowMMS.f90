
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

    DO jj = 2,numberOfGridPoints
    S_1(jj) = -i*(-10.0d0*0.0  )*(-0.0*(-2.0d0*r(jj)**3 + 3.0d0*r(jj)**2) - 0.0*( &
      2.0d0*r(jj)**3 - 3.0d0*r(jj)**2 + 1) + 0.0) - 6.0d0*r(jj)**2 + 6.0d0*r(jj)
S_2(jj) = 7.0d0*i*(-2.0d0*r(jj)**3 + 3.0d0*r(jj)**2)/r(jj)
S_3(jj) = 10.0d0*i*(-2.0d0*r(jj)**3 + 3.0d0*r(jj)**2)
S_4(jj) = -0.0*(-6.0d0*r(jj)**2 + 6.0d0*r(jj)) - 0.0*(6.0d0*r(jj)**2 - 6.0d0*r(jj)) - i*(-10.0d0* &
      0.0  )*(-2.0d0*r(jj)**3 + 3.0d0*r(jj)**2) + 1.0d0*(-0.0*( &
      -2.0d0*r(jj)**3 + 3.0d0*r(jj)**2) - 0.0*(2.0d0*r(jj)**3 - 3.0d0*r(jj)**2 + 1) + &
      0.0)/r(jj)

    END DO
    

    S_1(1) = 0
    S_2(1) = 0
S_3(1) = 10.0d0*i*(-2.0d0*r(1)**3 + 3.0d0*r(1)**2)
S_4(1) = 10.0d0*i*(-2.0d0*r(1)**3 + 3.0d0*r(1)**2)

    
    END SUBROUTINE SourceCalc
    
