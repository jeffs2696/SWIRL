 
    ! gam - axial wavenumber t
    ! ak  - reduced frequency
    ! i - imaginary number
     
        SUBROUTINE SourceCalcComponents(& 
        gam  , &
        i    , &
        ak   , &
        m    , & 
        r    , &
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
        REAL(KIND=rDef)   , INTENT(IN) :: r
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
    S_A11=-i*(1.0d0/(0.125d0*tanh(0.033333333333333333d0*r - &
      0.033333333333333333d0) + 1) - 3.16227766016838d0*sqrt(r*( &
      0.00833333333333333d0 - 0.00833333333333333d0*tanh( &
      0.033333333333333333d0*r - 0.033333333333333333d0)**2)*1d0/( &
      0.125d0*tanh(0.033333333333333333d0*r - 0.033333333333333333d0) + &
      1))/r)*(0.0102849805345748d0*(r - 0.1d0)**3 - &
      0.0138847237216759d0*(r - 0.1d0)**2 + 0.125d0*tanh( &
      0.033333333333333333d0*r - 0.033333333333333333d0) + &
      0.00374887540485247d0)
S_A12=-3.16227766016838d0*sqrt(r*(0.00833333333333333d0 - &
      0.00833333333333333d0*tanh(0.033333333333333333d0*r - &
      0.033333333333333333d0)**2)*1d0/(0.125d0*tanh( &
      0.033333333333333333d0*r - 0.033333333333333333d0) + 1))*(0.125d0 &
      *tanh(0.033333333333333333d0*r - 0.033333333333333333d0) + 1)/r
S_A13=0
S_A14=(1.0d0*(-4.6059873148567d0*r + 4.34975819681542d0*(r - 0.1d0)**2 - &
      0.00416666666666667d0*tanh(0.033333333333333333d0*r - &
      0.033333333333333333d0)**2 + 1.36302497669769d0)/( &
      0.89825957854535d0*r + 1.44991939893847d0*(r - 0.1d0)**3 - &
      2.30299365742835d0*(r - 0.1d0)**2 + 0.125d0*tanh( &
      0.033333333333333333d0*r - 0.033333333333333333d0) + &
      0.910174042145465d0) + 1.0d0*(r*(0.00833333333333333d0 - &
      0.00833333333333333d0*tanh(0.033333333333333333d0*r - &
      0.033333333333333333d0)**2)*1d0/(0.125d0*tanh( &
      0.033333333333333333d0*r - 0.033333333333333333d0) + 1))**1.0d0/r &
      )*(0.89825957854535d0*r + 1.44991939893847d0*(r - 0.1d0)**3 - &
      2.30299365742835d0*(r - 0.1d0)**2 + 0.125d0*tanh( &
      0.033333333333333333d0*r - 0.033333333333333333d0) + &
      0.910174042145465d0)
S_A21=(1.58113883008419d0*sqrt(r*(0.00833333333333333d0 - &
      0.00833333333333333d0*tanh(0.033333333333333333d0*r - &
      0.033333333333333333d0)**2)*1d0/(0.125d0*tanh( &
      0.033333333333333333d0*r - 0.033333333333333333d0) + 1))/r + &
      1.58113883008419d0*sqrt(r*(0.00833333333333333d0 - &
      0.00833333333333333d0*tanh(0.033333333333333333d0*r - &
      0.033333333333333333d0)**2)*1d0/(0.125d0*tanh( &
      0.033333333333333333d0*r - 0.033333333333333333d0) + 1))*(0.125d0 &
      *tanh(0.033333333333333333d0*r - 0.033333333333333333d0) + 1)** &
      1.0d0*(0.5d0*r*(0.00833333333333333d0 - 0.00833333333333333d0* &
      tanh(0.033333333333333333d0*r - 0.033333333333333333d0)**2)*( &
      0.125d0*tanh(0.033333333333333333d0*r - 0.033333333333333333d0) + &
      1)**(-2.0d0)*(0.00416666666666667d0*tanh(0.033333333333333333d0*r &
      - 0.033333333333333333d0)**2 - 0.00416666666666667d0) - &
      0.00416666666666667d0*r*(0.0666666666666667d0 - &
      0.0666666666666667d0*tanh(0.033333333333333333d0*r - &
      0.033333333333333333d0)**2)*1d0/(0.125d0*tanh( &
      0.033333333333333333d0*r - 0.033333333333333333d0) + 1)*tanh( &
      0.033333333333333333d0*r - 0.033333333333333333d0) + 0.5d0*( &
      0.00833333333333333d0 - 0.00833333333333333d0*tanh( &
      0.033333333333333333d0*r - 0.033333333333333333d0)**2)*1d0/( &
      0.125d0*tanh(0.033333333333333333d0*r - 0.033333333333333333d0) + &
      1))/(r*(0.00833333333333333d0 - 0.00833333333333333d0*tanh( &
      0.033333333333333333d0*r - 0.033333333333333333d0)**2)) + &
      0.790569415042095d0*(r*(0.00833333333333333d0 - &
      0.00833333333333333d0*tanh(0.033333333333333333d0*r - &
      0.033333333333333333d0)**2)*1d0/(0.125d0*tanh( &
      0.033333333333333333d0*r - 0.033333333333333333d0) + 1))**1.5d0/r &
      )*(0.0102849805345748d0*(r - 0.1d0)**3 - 0.0138847237216759d0*(r &
      - 0.1d0)**2 + 0.125d0*tanh(0.033333333333333333d0*r - &
      0.033333333333333333d0) + 0.00374887540485247d0)
S_A22=-i*(1.0d0/(0.125d0*tanh(0.033333333333333333d0*r - &
      0.033333333333333333d0) + 1) - 3.16227766016838d0*sqrt(r*( &
      0.00833333333333333d0 - 0.00833333333333333d0*tanh( &
      0.033333333333333333d0*r - 0.033333333333333333d0)**2)*1d0/( &
      0.125d0*tanh(0.033333333333333333d0*r - 0.033333333333333333d0) + &
      1))/r)*(0.125d0*tanh(0.033333333333333333d0*r - &
      0.033333333333333333d0) + 1)
S_A23=0
S_A24=2.0d0*i*(0.89825957854535d0*r + 1.44991939893847d0*(r - 0.1d0)**3 - &
      2.30299365742835d0*(r - 0.1d0)**2 + 0.125d0*tanh( &
      0.033333333333333333d0*r - 0.033333333333333333d0) + &
      0.910174042145465d0)/r
S_A31=(-0.000833333333333333d0*tanh(0.033333333333333333d0*r - &
      0.033333333333333333d0)**2 + 0.000833333333333333d0 + 0.5d0*(r*( &
      0.00833333333333333d0 - 0.00833333333333333d0*tanh( &
      0.033333333333333333d0*r - 0.033333333333333333d0)**2)*1d0/( &
      0.125d0*tanh(0.033333333333333333d0*r - 0.033333333333333333d0) + &
      1))**1.0d0*(0.025d0*tanh(0.033333333333333333d0*r - &
      0.033333333333333333d0) + 0.2d0)/r)*(0.0102849805345748d0*(r - &
      0.1d0)**3 - 0.0138847237216759d0*(r - 0.1d0)**2 + 0.125d0*tanh( &
      0.033333333333333333d0*r - 0.033333333333333333d0) + &
      0.00374887540485247d0)
S_A32=0
S_A33=-i*(1.0d0/(0.125d0*tanh(0.033333333333333333d0*r - &
      0.033333333333333333d0) + 1) - 3.16227766016838d0*sqrt(r*( &
      0.00833333333333333d0 - 0.00833333333333333d0*tanh( &
      0.033333333333333333d0*r - 0.033333333333333333d0)**2)*1d0/( &
      0.125d0*tanh(0.033333333333333333d0*r - 0.033333333333333333d0) + &
      1))/r)*(0.125d0*tanh(0.033333333333333333d0*r - &
      0.033333333333333333d0) + 1)
S_A34=0
S_A41=(1.0d0*(-0.0277694474433519d0*r + 0.0308549416037245d0*(r - 0.1d0)**2 - &
      0.00416666666666667d0*tanh(0.033333333333333333d0*r - &
      0.033333333333333333d0)**2 + 0.00694361141100185d0)/( &
      0.0102849805345748d0*(r - 0.1d0)**3 - 0.0138847237216759d0*(r - &
      0.1d0)**2 + 0.125d0*tanh(0.033333333333333333d0*r - &
      0.033333333333333333d0) + 0.00374887540485247d0) + 3.0d0*(r*( &
      0.00833333333333333d0 - 0.00833333333333333d0*tanh( &
      0.033333333333333333d0*r - 0.033333333333333333d0)**2)*1d0/( &
      0.125d0*tanh(0.033333333333333333d0*r - 0.033333333333333333d0) + &
      1))**1.0d0/r + 1.0d0/r)*(0.0102849805345748d0*(r - 0.1d0)**3 - &
      0.0138847237216759d0*(r - 0.1d0)**2 + 0.125d0*tanh( &
      0.033333333333333333d0*r - 0.033333333333333333d0) + &
      0.00374887540485247d0)
S_A42=2.0d0*i*(0.125d0*tanh(0.033333333333333333d0*r - 0.033333333333333333d0 &
      ) + 1)/r
S_A43=0
S_A44=-i*(1.0d0/(0.125d0*tanh(0.033333333333333333d0*r - &
      0.033333333333333333d0) + 1) - 3.16227766016838d0*sqrt(r*( &
      0.00833333333333333d0 - 0.00833333333333333d0*tanh( &
      0.033333333333333333d0*r - 0.033333333333333333d0)**2)*1d0/( &
      0.125d0*tanh(0.033333333333333333d0*r - 0.033333333333333333d0) + &
      1))/r)*(0.89825957854535d0*r + 1.44991939893847d0*(r - 0.1d0)**3 &
      - 2.30299365742835d0*(r - 0.1d0)**2 + 0.125d0*tanh( &
      0.033333333333333333d0*r - 0.033333333333333333d0) + &
      0.910174042145465d0)
S_B11=-1.0d0*i*(0.025d0*tanh(0.033333333333333333d0*r - 0.033333333333333333d0 &
      ) + 0.2d0)*(0.0102849805345748d0*(r - 0.1d0)**3 - &
      0.0138847237216759d0*(r - 0.1d0)**2 + 0.125d0*tanh( &
      0.033333333333333333d0*r - 0.033333333333333333d0) + &
      0.00374887540485247d0)
S_B12=0
S_B13=0
S_B14=0
S_B21=0
S_B22=-1.0d0*i*(0.025d0*tanh(0.033333333333333333d0*r - 0.033333333333333333d0 &
      ) + 0.2d0)*(0.125d0*tanh(0.033333333333333333d0*r - &
      0.033333333333333333d0) + 1)
S_B23=0
S_B24=0
S_B31=0
S_B32=0
S_B33=-1.0d0*i*(0.025d0*tanh(0.033333333333333333d0*r - 0.033333333333333333d0 &
      ) + 0.2d0)*(0.125d0*tanh(0.033333333333333333d0*r - &
      0.033333333333333333d0) + 1)
S_B34=-1.0d0*i*(0.89825957854535d0*r + 1.44991939893847d0*(r - 0.1d0)**3 - &
      2.30299365742835d0*(r - 0.1d0)**2 + 0.125d0*tanh( &
      0.033333333333333333d0*r - 0.033333333333333333d0) + &
      0.910174042145465d0)
S_B41=0
S_B42=0
S_B43=-1.0d0*i*(0.125d0*tanh(0.033333333333333333d0*r - 0.033333333333333333d0 &
      ) + 1)
S_B44=-1.0d0*i*(0.025d0*tanh(0.033333333333333333d0*r - 0.033333333333333333d0 &
      ) + 0.2d0)*(0.89825957854535d0*r + 1.44991939893847d0*(r - 0.1d0) &
      **3 - 2.30299365742835d0*(r - 0.1d0)**2 + 0.125d0*tanh( &
      0.033333333333333333d0*r - 0.033333333333333333d0) + &
      0.910174042145465d0)

    
    
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
    