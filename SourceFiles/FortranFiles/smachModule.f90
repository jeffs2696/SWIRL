MODULE smachModule
      USE, INTRINSIC :: ISO_FORTRAN_ENV
      ! USE Akima1D
      IMPLICIT NONE
      PRIVATE
      PUBLIC :: smach

INTERFACE smach
      MODULE PROCEDURE smach1
END INTERFACE smach

      INTEGER, PARAMETER :: rDef = REAL64

CONTAINS

      subroutine smach1(npts,rr,rmsw,rmswp,snd,dsn,dd,angom,gam,rro,is)

      INTEGER, INTENT(IN) :: npts, &
                               is

      REAL(KIND=rDef), INTENT(IN) :: angom, &
                                     gam,   &
                                     rro

      REAL(KIND=rDef), DIMENSION(:), INTENT(IN) :: rr, &
                                                  snd, &
                                                  dsn

      REAL(KIND=rDef), DIMENSION(:,:), INTENT(IN) :: dd

      REAL(KIND=rDef), DIMENSION(:), INTENT(OUT) :: rmsw,  &
                                                    rmswp
!
! local variables
!
      INTEGER :: i, &
                 j, &
                 k

      REAL(KIND=rDef) :: r, &
                       tot

      REAL(KIND=rDef), DIMENSION(npts) :: vsw, &
                                         vswp

      INTEGER :: nptsIn
      REAL(KIND=rDef), DIMENSION(:), ALLOCATABLE :: rIn, &
                                                 rmswIn
!
!     implicit real*8 (a-h,o-z)
!     parameter (NMAX = 128)
!     dimension rmsw(NMAX),rmswp(NMAX),dd(NMAX,NMAX),rr(NMAX)
!     dimension vsw(1024),vswp(1024)
!     dimension snd(NMAX),dsn(NMAX)
!
!  is = 0: no swirl
!  is = 1: solid-body swirl; ang. vel. angom
!  is = 2: free-vortex swirl; gam/r
!  is = 3: solid-body and free-vortex swirl.
!  is = 4: Vt = 1/r^2; put in for stability tests.
!  is = 5: read in swirl from file.
!  is = 6: constant swirl across the duct; Vt = const.
!  is = 7: trailing line vortex; Vt = Vmax/r (1 - e^(-r^2))
!
!  is = 0: no swirl
!
      if (is.eq.0) then
       do i = 1,npts
        rmsw(i)  = 0.0_rDef
        rmswp(i) = 0.0_rDef
       enddo
!
!  is = 1: solid-body swirl; ang. vel. angom
!
      elseif (is.eq.1) then
       do i = 1,npts
        r        =  rr(i)
        vsw(i)   =  angom*r
        vswp(i)  =  angom
        rmsw(i)  =  vsw(i)/snd(i)
        rmswp(i) =  angom/snd(i)*(1.0_rDef -r/snd(i)*dsn(i))
       enddo
!
!  is = 2: free-vortex swirl; gam/r
!
      elseif (is.eq.2) then
       do i = 1,npts
        r        =  rr(i)
        vsw(i)   =  gam/r
        vswp(i)  = -gam/(r*r)
        rmsw(i)  =  vsw(i)/snd(i)
        rmswp(i) = -gam/(snd(i)*snd(i)*r*r)*(snd(i) +r*dsn(i))
       enddo
!
!  is = 3: solid-body and free-vortex swirl.
!
      elseif (is.eq.3) then
       do i = 1,npts
        r        =  rr(i)
        vsw(i)   =  angom*r +gam/r
        rmsw(i)  =  vsw(i)/snd(i)
        vswp(i)  =  angom -gam/(r*r)
        rmswp(i) =  angom/snd(i)*(1.0_rDef -r/snd(i)*dsn(i)) &
           -gam/(snd(i)*snd(i)*r*r)*(snd(i) +r*dsn(i))
       enddo
!
!  is = 4: Vt = 1/r^2; put in for stability tests.
!
      elseif (is.eq.4) then
       do i = 1,npts
        r        =  rr(i)
        vsw(i)   =  gam/(r*r)
        rmsw(i)  =  vsw(i)/snd(i)
        vswp(i)  = -2.0_rDef*gam/(r*r*r)
        rmswp(i) = -gam/(r**3*snd(i)**2)*(2.0_rDef*snd(i) +r*dsn(i))
       enddo
!
!  is = 5: read in swirl from file.
!
      elseif (is.eq.5) then
       open(unit=24,file='swrl.input',status='unknown')
       READ(24,*) nptsIn
       ALLOCATE(rIn(nptsIn), &
             rmswIn(nPtsIn))
       DO i=1,nptsIn
        READ(24,*) rIn(i),rmsw(i)
       END DO
!      read (24,*) (rmsw(i), i = 1,npts)
       close(24)
!
! spline data onto grid
!

       ! CALL Akima433Interpolation(inputDataLength  = nptsIn, &
       !                            xInputData       = rIn,    &
       !                            yInputData       = rmswIn, &
       !                            outputDataLength = npts,   &
       !                            xOutputData      = rr,     &
       !                            yOutputData      = rmsw)

       DEALLOCATE(rIn, rmswIn)
!
! Spectral computation of M_theta'.
       do k=1,npts
        tot = 0.0_rDef
        do j=1,npts
         tot = tot +dd(k,j)*rmsw(j)
        enddo
        rmswp(k) = tot
       enddo
!
!  is = 6: constant swirl across the duct; Vt = const.
!
      elseif (is.eq.6) then
       do i = 1,npts
        r        =  rr(i)
        vsw(i)   =  angom
        rmsw(i)  =  vsw(i)/snd(i)
        vswp(i)  =  0.0_rDef
        rmswp(i) = -angom/snd(i)*dsn(i)
       enddo
!
!  is = 7: trailing line vortex; Vt = Vmax/r (1 - e^(-r^2))
!
      elseif (is.eq.7) then
       do i = 1,npts
        r        =  rr(i)
        vsw(i)   =  gam/r*(1.0_rDef -exp(-r*r))
        rmsw(i)  =  vsw(i)/snd(i)
        vswp(i)  =  2.0_rDef*gam*exp(-r*r) -gam/(r*r)*(1.0_rDef -exp(r*r))
!
! Need to add rmswp calculation here.
!
       enddo
      endif
!
! Divide swirl velocity by speed of sound to get swirl Mach number.
!$$$      if (is.ne.5) then
!$$$       do i = 1,npts
!$$$        rmsw(i)  = vsw(i)/snd(i)
!$$$        rmswp(i) = vswp(i)/snd(i) -vsw(i)*dsn(i)/(snd(i)*snd(i))
!$$$       enddo
!$$$      endif
!
      return
      WRITE(6,*) rro
      end
END MODULE smachModule
