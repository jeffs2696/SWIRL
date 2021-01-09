MODULE gridModule
   USE, INTRINSIC :: ISO_FORTRAN_ENV
   IMPLICIT NONE

   PRIVATE
   PUBLIC :: grid

INTERFACE grid
  MODULE PROCEDURE grid1
END INTERFACE

   INTEGER, PARAMETER :: rDef = REAL64

CONTAINS

      subroutine grid1(np,sig,x,r)
!
!  COMPUTE THE CHEBYSHEV GAUSS-LOBATTO GRID, reversed.
!    See Canuto, p. 67.
!
!     implicit real*8 (a-h,o-z)
!     parameter (NMAX = 128)

      INTEGER, INTENT(IN) :: np
      REAL(KIND=rDef), INTENT(IN) :: sig
      REAL(KIND=rDef), DIMENSION(:), INTENT(OUT) :: x, & ! np
                                                    r    ! np
      
! local variables

      INTEGER :: n,j

      REAL(KIND=rDef) :: alpha,  &
                         zero,   &
                         sina,   &
                         pi,     &
                         pin,    &
                         sigbar, &
                         coeff,  &
                         yj

!     real*8 x(NMAX),r(NMAX),alpha,zero,sina
!
      WRITE(6,*) 'Entering grid'

      PI     = 4.0_rDef*ATAN(1.0_rDef)
      n      = np -1
      PIN    = PI/REAL(N,rDef)
      zero   = 1.11e-16_rDef
      alpha  = 1.0_rDef/cosh(abs(log(zero))/REAL(n,rDef))
      sina   = asin(alpha)
      sigbar = (1.0_rDef +sig)/(1.0_rDef -sig)
      coeff  = 0.50_rDef*(1.0_rDef -sig)

      DO j=1,NP
       x(j)  = -COS(REAL((j-1),rDef)*PIN)
       yj    =  asin(-x(j)*alpha)/sina
       r(j)  =  coeff*(-yj +sigbar)
      END DO

      WRITE(6,*) 'Leaving grid'
!
      return
      end SUBROUTINE grid1

END MODULE gridModule
