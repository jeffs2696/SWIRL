MODULE boundaryModule
   USE, INTRINSIC :: ISO_FORTRAN_ENV
   IMPLICIT NONE
   PRIVATE
   PUBLIC :: boundary

INTERFACE boundary
  MODULE PROCEDURE boundary1
END INTERFACE

   INTEGER, PARAMETER :: rDef = REAL64

CONTAINS

      subroutine boundary1(np,sig,ak,etah,etad,rmx,rmt,dd,aa,bb)

      INTEGER, INTENT(IN) :: np
      REAL(KIND=rDef), INTENT(IN) :: sig

      COMPLEX(KIND=rDef), INTENT(IN) :: ak, &
                                      etah, &
                                      etad

      REAL(KIND=rDef), DIMENSION(:), INTENT(IN) :: rmx, &
                                                   rmt

      REAL(KIND=rDef), DIMENSION(:,:), INTENT(IN) :: dd

      COMPLEX(KIND=rDef), DIMENSION(:,:), INTENT(INOUT) :: aa, &
                                                           bb

! local variables

      INTEGER :: j

      REAL(KIND=rDef) :: eps, &
                      etamag, &
                         rmh, &
                         rmd

      COMPLEX(KIND=rDef) :: ci

!
!     implicit real*8 (a-h,o-z)
!     parameter (NMAX = 128)
!
!     real*8     dd(NMAX,NMAX),rmx(NMAX),rmt(NMAX)
!     complex*16 aa(4*NMAX,4*NMAX),bb(4*NMAX,4*NMAX),etah,etad,ci,ak
!
! Boundary conditions.
      ci      = CMPLX(0.0_rDef,1.0_rDef,rDef)
      eps     = 1.e-4_rDef
!
      etamag = abs(etah) +abs(etad)

! is there a liner?

! go through these later.

      if (etamag.gt.eps) then
       rmh    = sqrt(rmx(1)*rmx(1)   +rmt(1)*rmt(1))
       rmd    = sqrt(rmx(np)*rmx(np) +rmt(np)*rmt(np))
       aa(np,np)   = ci*ak
       aa(np,2*np) = 0.0_rDef
       do j=1,np
        aa(np,3*np+j) = 0.0_rDef
       enddo
       aa(np,4*np) = -ci*ak*etad
       bb(np,np)   = 0.0_rDef
       bb(np,4*np) = etad*rmd

       if (sig.ne.0.0_rDef) then
        aa(1,1)    = -ci*ak
        aa(1,np+1) = 0.0_rDef
        do j=1,np
         aa(1,3*np+j) = 0.0_rDef
        enddo
        aa(1,3*np+1) = -ci*ak*etah
        bb(1,1)      = 0.0_rDef
        bb(1,3*np+1) = etah*rmh
       endif

      else

! no liner; hard wall conditions

       if (sig.ne.0.0_rDef) then ! there is an inner wall : BC: v_r = 0
        do j = 1,np
         aa(1,  np+j) = 0.0_rDef ! v_r eqn at inner wall, v_th entries
         aa(1,3*np+j) = 0.0_rDef ! v_r eqn at inner wall, p entries
         bb(1,     j) = 0.0_rDef ! v_r eqn at inner wall, v_r entries
        enddo
       endif

       do j = 1,np               ! outer wall : BC: v_r = 0
        aa(np,  np+j) = 0.0_rDef ! v_r eqn at outer wall, v_th entries
        aa(np,3*np+j) = 0.0_rDef ! v_r eqn at outer wall, p entries
        bb(np,     j) = 0.0_rDef ! v_r eqn at outer wall, v_r entries
       enddo
      endif
!
      return
      IF (MAXVAL(dd) > 0.0_rDef) CONTINUE
      end

END MODULE boundaryModule
