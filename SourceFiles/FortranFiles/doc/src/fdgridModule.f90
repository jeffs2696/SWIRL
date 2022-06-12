MODULE fdgridModule

  USE, INTRINSIC :: ISO_FORTRAN_ENV
  IMPLICIT NONE
  
  PRIVATE
  PUBLIC :: fdgrid

INTERFACE fdgrid
  MODULE PROCEDURE fdgrid1
END INTERFACE

  INTEGER, PARAMETER :: rDef = REAL64

CONTAINS

      subroutine fdgrid1(np,sig,x,r)
!
! Finite difference grid; equally spaced.
!

      INTEGER, INTENT(IN) :: np
      REAL(KIND=rDef), INTENT(IN) :: sig
      REAL(KIND=rDef), DIMENSION(:), INTENT(OUT) :: x, &
                                                    r

! define local variables

      INTEGER :: i
      REAL(KIND=rDef) :: sigbar, &
                         coeff,  &
                         dx
     
!     implicit real*8 (a-h,o-z)
!     parameter (NMAX = 128, NMAX4 = NMAX*4)
!     dimension r(NMAX),x(NMAX)
!
      sigbar = (1.0_rDef +sig)/(1.0_rDef -sig)
      coeff  = 0.5_rDef*(1.0_rDef -sig)
      dx = 2.0_rDef/REAL(np -1,rDef)
      do i = 1,np
       x(i) = -1.0_rDef +REAL(i-1,rDef)*dx
       r(i) = coeff*(x(i) +sigbar)
      end do
!
      return
      end

END MODULE fdgridModule
