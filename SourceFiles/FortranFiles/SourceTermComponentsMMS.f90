 
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
    S_A11=-10.0d0*i*(-0.0*(-2.0d0*r**3 + 3.0d0*r**2) - 0.0*(2.0d0*r**3 - 3.0d0*r** &
      2 + 1) + 0.0)/0.0
S_A12=0
S_A13=0
S_A14=-6.0d0*r**2 + 6.0d0*r
S_A21=0
S_A22=0
S_A23=0
S_A24=7.0d0*i*(-2.0d0*r**3 + 3.0d0*r**2)/r
S_A31=0
S_A32=0
S_A33=0
S_A34=0
S_A41=(1.0d0*(-0.0*(-6.0d0*r**2 + 6.0d0*r) - 0.0*(6.0d0*r**2 - 6.0d0*r))/(-0.0 &
      *(-2.0d0*r**3 + 3.0d0*r**2) - 0.0*(2.0d0*r**3 - 3.0d0*r**2 + 1) + &
      0.0) + 1.0d0/r)*(-0.0*(-2.0d0*r**3 + 3.0d0*r**2) - 0.0*(2.0d0*r** &
      3 - 3.0d0*r**2 + 1) + 0.0)
S_A42=0
S_A43=0
S_A44=-10.0d0*i*(-2.0d0*r**3 + 3.0d0*r**2)/0.0
S_B11=-10.0d0*0.0*i*(-0.0*(-2.0d0*r**3 + 3.0d0*r**2) - 0.0*(2.0d0*r**3 - 3.0d0 &
      *r**2 + 1) + 0.0)
S_B12=0
S_B13=0
S_B14=0
S_B21=0
S_B22=0
S_B23=0
S_B24=0
S_B31=0
S_B32=0
S_B33=0
S_B34=-10.0d0*i*(-2.0d0*r**3 + 3.0d0*r**2)
S_B41=0
S_B42=0
S_B43=0
S_B44=-10.0d0*0.0*i*(-2.0d0*r**3 + 3.0d0*r**2)

    
    
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
    