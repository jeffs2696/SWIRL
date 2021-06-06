MODULE sndspdModule
      USE, INTRINSIC :: ISO_FORTRAN_ENV
      IMPLICIT NONE
      PRIVATE
      PUBLIC :: sndspd

INTERFACE sndspd
  MODULE PROCEDURE sndspd1
END INTERFACE sndspd

      INTEGER, PARAMETER :: rDef = REAL64

CONTAINS

      subroutine sndspd1(np,rr,rmsw,asnd,dsnd,dd,rhob,angom,gam,sig,is)

      INTEGER, INTENT(IN) :: np, &
                             is

      REAL(KIND=rDef), DIMENSION(:), INTENT(IN) :: rr, &
                                                 rmsw, &
                                                 rhob

      REAL(KIND=rDef), DIMENSION(:,:), INTENT(IN) :: dd

      REAL(KIND=rDef), INTENT(IN) :: angom, &
                                       gam, &
                                       sig

      REAL(KIND=rDef), DIMENSION(:), INTENT(OUT) :: asnd, &
                                                    dsnd

!
! local variables
!
      INTEGER :: j, &
                 k

      REAL(KIND=rDef) :: gm, &
                        gm1, &
                          r, &
                        ang, &
                        agm, &
                        alg, &
                        tot
!
!     implicit real*8 (a-h,o-z)
!     parameter (NMAX = 128)
!     dimension rr(NMAX),rmsw(NMAX),asnd(NMAX),dsnd(NMAX),rhob(NMAX)
!     dimension dd(NMAX,NMAX)
!
      gm  = 1.4_rDef
      gm1 = gm -1.0_rDef
      do j = 1,np
        r  = rr(j)
!
! is = 0: no swirl
!
        if (is.eq.0) then
          asnd(j) =  1.0_rDef
          dsnd(j) =  0.0_rDef
!
! is = 1: solid-body swirl
!
        elseif (is.eq.1) then
          asnd(j) =  1.0_rDef -gm1/2.*angom*angom*(1.0_rDef -r*r)
          asnd(j) =  sqrt(asnd(j))
          dsnd(j) =  gm1*angom*angom*r/(2.*asnd(j))
!
! is = 2: free-vortex swirl
!
        elseif (is.eq.2) then
          asnd(j) =  1. -gm1/2.*gam*gam*(1./(r*r) -1.)
          asnd(j) =  sqrt(asnd(j))
          dsnd(j) =  gm1*gam*gam/(2.*r*r*r*asnd(j))
!
! is = 3: both solid-body and free-vortex swirl
!
        elseif (is.eq.3) then
          ang     = -gm1/2.*angom*angom*(1. -r*r)
          agm     = -gm1/2.*gam*gam*(1./(r*r) -1.)
          alg     =  2*gm1*angom*gam*log(r)
          asnd(j) =  1. +ang +agm +alg
          asnd(j) =  sqrt(asnd(j))
          dsnd(j) =  gm1/2.*(angom*r +gam/r)**2/(r*asnd(j))
!
! is = 4: Vt = 1/r^2; put in for stability tests
!
        elseif (is.eq.4) then
          asnd(j) =  1. -gm1/4.*gam*gam*(1./(r**4) -1.)
          asnd(j) =  sqrt(asnd(j))
          dsnd(j) =  gm1*gam*gam/(2.*r**5*asnd(j))
!
! is = 6: constant swirl across the duct
!
        elseif (is.eq.6) then
          asnd(j) = 1. +gm1*angom*angom*log(r)
          asnd(j) = sqrt(asnd(j))
          dsnd(j) = gm1*angom*angom/(2.*r*asnd(j))
!
! otherwise:  integrate to find the speed of sound
! -------this was commented out in the original code--------
!
        else 
         asnd(j) = 0.
         do i = np,j,-1
          if (i.eq.np) then
           asnd(i) = 0.
          else
           if (rr(i).ne.0.) then
            rswi    = rmsw(i)*rmsw(i)/rr(i)
            rsw1    = rmsw(i+1)*rmsw(i+1)/rr(i+1)
            xi      = rr(i)
            x1      = rr(i+1)
            asnd(i) = asnd(i+1) +0.5_rDef*(rswi +rsw1)*(xi -x1)
           else
            asnd(i) = 2.0_rDef*rmsw(i)*(rmsw(i+1) -rmsw(i))/rr(i+1)
           endif
          endif
         enddo
         asnd(j) = exp(-gm1*asnd(j))
        endif

      enddo
!
! is = 5: read in swirl from file.
!
      if (is.eq.5) then
        do k = 1,np
          tot = 0.
          do j = 1,np
            tot = tot +dd(k,j)*asnd(j)
          enddo
          dsnd(k) = tot
        enddo
      endif
!
      return
      WRITE(6,*) rhob,rmsw,sig
      end

END MODULE sndspdModule
