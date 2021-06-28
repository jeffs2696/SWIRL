MODULE rmachModule
      USE, INTRINSIC :: ISO_FORTRAN_ENV
      ! USE Akima1D
      IMPLICIT NONE
      PRIVATE
      PUBLIC :: rmach

INTERFACE rmach
      MODULE PROCEDURE rmach1
END INTERFACE

      INTEGER, PARAMETER :: rDef = REAL64

CONTAINS

      subroutine rmach1(npts,rr,rmch,drm,snd,dsn,dd,rxmax,slope,rro,ir)

      INTEGER, INTENT(IN) :: npts
      REAL(KIND=rDef), DIMENSION(:), INTENT(IN) :: rr, &
                                                  snd, &
                                                  dsn

      REAL(KIND=rDef), DIMENSION(:), INTENT(OUT) :: drm, &
                                                   rmch

      REAL(KIND=rDef), DIMENSION(:,:), INTENT(IN) :: dd

      REAL(KIND=rDef), INTENT(OUT) :: slope

      REAL(KIND=rDef), INTENT(IN) :: rxmax, &
                                       rro

      INTEGER, INTENT(INOUT) :: ir
!
! local variables
!

      INTEGER :: &
          i, &
          j, &
          k

      REAL(KIND=rDef) :: pi, &
                          r, &
                      acnst, &
                     acoeff, &
                      acsh2, &
                        arg, &
                      bcnst, &
                     bcoeff, &
                     ccoeff, &
                       cnst, &
                       dels, &
                      delta, &
                       den1, &
                       den2, &
                       dnom, &
                        pi2, &
                        pi4, &
                     plsmns, &
                        pwl, &
                       reta, &
                       rmax, &
                       rmin, &
                        rsh, &
                        rsi, &
                        sgn, &
                        tot, &
                          y

      REAL(KIND=rDef), DIMENSION(npts) :: vrm, &
                                         vrmp

      ! REAL(KIND=rDef), DIMENSION(:), ALLOCATABLE :: rIn, &
      !                                            rmchIn

!
!     implicit real*8 (a-h,o-z)
!     parameter (NMAX = 128)
!     dimension rmch(NMAX),drm(NMAX),dd(NMAX,NMAX),rr(NMAX)
!     dimension vrm(1024),vrmp(1024)
!     dimension snd(NMAX),dsn(NMAX)
!
!  ir = 0 : uniform
!  ir = 1 : linear shear
!  ir = 2 : read shear distribution from a file
!  ir = 3 : uniform + sine wave boundary layers of thickness delta
!  ir = 4 : uniform + linear b.l.'s
!  ir = 5 : uniform + 1/7th power law b.l.'s
!  ir = 6 : hyperbolic secant profile, M(r) = M_0 sech(2/del*cosh^-1(2)r)
!  ir = 7 : laminar mean flow, M(r) = M_0 (1 -r^2)
!  ir = 8 : wavy sinusoid, M(r) = (M_max +M_min)/2 
!                           +(M_max -M_min)/2*sin(4*PI*R)
!  ir = 9 : Hagen-Poiseuille flow, Vx(r) = 1 +K ln r - r^2
!
      pi = 4.0_rDef*ATAN(1.0_rDef)
!
!  ir = 0 : uniform
!
      if (ir.eq.0) then
       slope = 0.d0
       ir    = 1
      endif
!
!  ir = 1 : linear shear
!
      if (ir.eq.1) then
       do i = 1,npts
        r   = rr(i)
        if (slope.ge.0.0_rDef) then
         vrm(i) = slope*(r -1.0_rDef)  +rxmax
        else
         vrm(i) = slope*(r -rro) +rxmax
        endif 
        vrmp(i) = slope
       enddo
       do i = 1,npts
        drm(i)  = vrmp(i)/snd(i) -vrm(i)*dsn(i)/(snd(i)*snd(i)) !dM_x/dr
        rmch(i) = vrm(i)/snd(i)
       enddo
      endif
!
!  ir = 2 : read shear distribution from a file
!
      if (ir.eq.2) then
!
! drh mod:  read in data and use akima spline
!
       ! open(unit=22,file='mach.input',status='unknown')
       ! READ(22,*) nptsIn

       ! ALLOCATE(rIn(nptsIn), &
       !       rmchIn(nptsIn))

       ! DO i=1,nptsIn
       !  READ(22,*) rIn(i),rmchIn(i)
       ! END DO

       ! CLOSE(22)
!
! spline data onto grid
!
! JS: removed read from file capability, now it is input from main

       ! CALL Akima433Interpolation(inputDataLength  = nptsIn, &
       !                            xInputData       = rIn,    &
       !                            yInputData       = rmchIn, &
       !                            outputDataLength = npts,   &
       !                            xOutputData      = rr,     &
       !                            yOutputData      = rmch)

       ! DEALLOCATE(rIn, rmchIn)

!      read (22,*) (rmch(i), i = 1,npts)
!      close(22)
!
! Spectral computation of M'.
       do k=1,npts
        tot = 0.0_rDef
        do j=1,npts
         tot = tot +dd(k,j)*rmch(j)
        enddo
        drm(k) = tot
       enddo

      endif
!
!  ir = 3 : uniform + sine wave boundary layers of thickness delta
!
      if (ir.eq.3) then
       delta     = slope
       do i = 1,npts
        r        =  rr(i)
        if ((r.le.(rro +delta)) .and. rro.ne.0.0_rDef) then
         vrm(i)  =  rxmax*sin((r -rro)*PI/(2.0_rDef*delta))
         vrmp(i) =  rxmax*PI/(2.0_rDef*delta)*cos((r -rro)*PI/(2.0_rDef*delta))
        elseif (r.lt.(1.0_rDef -delta)) then
         vrm(i)  =  rxmax
         vrmp(i) =  0.0_rDef
        else
         vrm(i)  =  rxmax*sin((1.0_rDef -r)*PI/(2.0_rDef*delta))
         vrmp(i) = -rxmax*PI/(2.0_rDef*delta)*cos((1.0_rDef -r)*PI/(2.0_rDef*delta))
        endif
       enddo
       do i = 1,npts
        drm(i)  = vrmp(i)/snd(i) -vrm(i)*dsn(i)/(snd(i)*snd(i))
        rmch(i) = vrm(i)/snd(i)
       enddo
      endif
!
!  ir = 4 : uniform + linear b.l.'s
!
      if (ir.eq.4) then
       delta     = slope
       dels      = 2.0_rDef*delta/(1.0_rDef -rro)
       do i = 1,npts
        r        =  rr(i)
        reta     =  (2.0_rDef*r -rro -1.0_rDef)/(1.0_rDef -rro)
        if (reta.ge.(-1.0_rDef +dels) .and. reta.le.(1.0_rDef -dels)) then
         vrm(i)  =  rxmax/(1.0_rDef -0.50_rDef*dels)
         vrmp(i) =  0.0_rDef
        elseif (reta.gt.(1.0_rDef -dels)) then
         vrm(i)  =  rxmax*(1.0_rDef -reta)/(dels*(1.0_rDef -0.50_rDef*dels))
         vrmp(i) = -rxmax/(dels*(1.0_rDef -0.50_rDef*dels))
        elseif ((reta.lt.(-1.0_rDef +dels)).and. rro.ne.0.0_rDef) then
         vrm(i)  =  rxmax*(1.0_rDef +reta)/(dels*(1.0_rDef -0.50_rDef*dels))
         vrmp(i) =  rxmax/(dels*(1.0_rDef -0.50_rDef*dels))
        endif
       enddo
       do i = 1,npts
        drm(i)  = vrmp(i)/snd(i) -vrm(i)*dsn(i)/(snd(i)*snd(i))
        rmch(i) = vrm(i)/snd(i)
       enddo
      endif
!
!  ir = 5 : uniform + 1/7th power law b.l.'s
!
      if (ir.eq.5) then
       rsi      = rro/(1.0_rDef -rro)
       plsmns   = rsi +0.50_rDef
       do i = 2,npts-1
        r       = rr(i)
        rsh     = r/(1.0_rDef -rro)
        if (rro.gt.0.0_rDef) then ! annulus; equation changes.
                                  ! given in Shankar (Ref. 10)
         pwl    = 1.0_rDef -2.0_rDef*abs(rsi +0.50_rDef -rsh)
         vrm(i) = rxmax*pwl**(1.0_rDef/7.0_rDef)
         if (rsh.le.plsmns) then
          sgn  = -1.0_rDef
         else
          sgn  =  1.0_rDef
         endif
         vrmp(i) = -2.0_rDef*rxmax/(7.0_rDef*pwl**(6.0_rDef/7.0_rDef))*sgn
        else ! cylinder
         vrm(i)  =  rxmax*(1.0_rDef -rsh)**(1.0_rDef/7.0_rDef)
         vrmp(i) = -rxmax/7.0_rDef*(1.0_rDef -rsh)**(-6.0_rDef/7.0_rDef)
        endif
       enddo
       if (rro.ne.0.0_rDef) then ! need to go through the Shankar paper for this
        vrm(1)  = 0.0_rDef
        vrmp(1) = 1.10_rDef*vrmp(2)
       ELSE ! DRH addition; set value at centerline of cylinder (r=0)
        vrm(1)  = rxmax     
        vrmp(1) = 0.0_rDef 
       endif
       vrm(npts)  = 0.0_rDef
       vrmp(npts) = 1.10_rDef*vrmp(npts-1) ! ??
       do i = 1,npts
        drm(i)  = vrmp(i)/snd(i) -vrm(i)*dsn(i)/(snd(i)*snd(i))
        rmch(i) = vrm(i)/snd(i)
       enddo
      endif
!
!  ir = 6 : hyperbolic secant profile, M(r) = M_0 sech(2/del*cosh^-1(2)r)
!
      if (ir.eq.6) then
       acsh2 = log(2.00_rDef +sqrt(2.0_rDef*2.0_rDef -1.0_rDef))
       dels  = slope
       do i = 1,npts
        r       = rr(i)
        y       = 2.0_rDef*(r -rro)/(1.0_rDef -rro) -1.0_rDef
        acnst   = 2.0_rDef*acsh2/dels
        arg     = acnst*y
        rmch(i) = rxmax/cosh(arg)
        drm(i)  = -acnst*rxmax*tanh(arg)/cosh(arg)
       enddo
      endif
!
!  ir = 9 : Hagen-Poiseuille flow, Vx(r) = 1 +K ln r - r^2
!
      if (ir.eq.9) then
       if (rro.ne.0.0_rDef) then
        cnst = (rro*rro -1.0_rDef)/log(rro)
        dnom = 1.0_rDef +cnst/2.0_rDef*log(cnst/2.0_rDef) -cnst/2.0_rDef
        do i = 1,npts
         r       = rr(i)
         vrm(i)  = rxmax*(1.0_rDef +cnst*log(r) -r*r)/dnom
         vrmp(i) = cnst/r -2.0_rDef*r
        enddo
        do i = 1,npts
         drm(i)  = vrmp(i)/snd(i) -vrm(i)*dsn(i)/(snd(i)*snd(i))
         rmch(i) = vrm(i)/snd(i)
        enddo
       else
        ir = 7
       endif
      endif
!
!  ir = 7 : laminar mean flow, M(r) = M_0 (1 -r^2)
!
      if (ir.eq.7) then
       den1   = 1.0_rDef/(rro -1.0_rDef)
       den2   = 1.0_rDef/(1.0_rDef -rro)
       acoeff = 4.0_rDef*rxmax*den1*den2
       bcoeff = -acoeff*(1.0_rDef +rro)
       ccoeff = acoeff*rro
       do i = 1,npts
        r       = rr(i)
        vrm(i)  = acoeff*r*r +bcoeff*r +ccoeff
        vrmp(i) = 2.0_rDef*acoeff*r +bcoeff
       enddo
       do i = 1,npts
        drm(i)  = vrmp(i)/snd(i) -vrm(i)*dsn(i)/(snd(i)*snd(i))
        rmch(i) = vrm(i)/snd(i)
       enddo
      endif
!
!  ir = 8 : wavy sinusoid, M(r) =  (M_max +M_min)/2 
!                                +((M_max -M_min)/2)*sin(4*PI*R)
!
      if (ir.eq.8) then
       pi2   = 2.0_rDef*PI
       pi4   = 4.0_rDef*PI
       rmax  = rxmax
       rmin  = slope
       acnst = rmax +rmin
       bcnst = rmax -rmin
       do i = 1,npts
        r = rr(i)
        y = 2.0_rDef*(r -rro)/(1.0_rDef -rro) -1.0_rDef
        arg = pi4*y
        rmch(i) = 0.50_rDef*acnst +0.50_rDef*bcnst*sin(arg)
        drm(i)  = pi2*bcnst*cos(arg)
       enddo
      endif
!
      return
      end
END MODULE rmachModule
