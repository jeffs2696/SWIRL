MODULE interpModule
  USE, INTRINSIC :: ISO_FORTRAN_ENV
  USE derivsModule
  USE fdgridModule
  USE fdrivsModule
  USE gridModule
  IMPLICIT NONE
  PRIVATE
  PUBLIC :: interp

INTERFACE interp
  MODULE PROCEDURE interp1
END INTERFACE

INTERFACE
  REAL FUNCTION curv2(t,n,x,y,yp,sigma)
    INTEGER :: n
    REAL :: t
    REAL, DIMENSION(:) :: x,y,yp
    REAL :: sigma
  END FUNCTION curv2
END INTERFACE

  INTEGER, PARAMETER :: rDef = REAL64, &
                        rSP  = REAL32

CONTAINS

      subroutine interp1(np,sig,rr,rmx,drm,rmt,drt,snd,dsn,dd, &
           ifdff,ed2,ed4)

      INTEGER, INTENT(IN) :: np, &
                          ifdff

      REAL(KIND=rDef), INTENT(OUT) :: sig

      REAL(KIND=rDef), INTENT(IN) :: ed2, &
                                     ed4

      REAL(KIND=rDef), DIMENSION(:), INTENT(OUT) :: rr, &
                                                   rmx, &
                                                   rmt, &
                                                   snd

      REAL(KIND=rDef), DIMENSION(:), INTENT(IN) :: drm, &
                                                   drt, &
                                                   dsn

      REAL(KIND=rDef), DIMENSION(:,:), INTENT(INOUT) :: dd

!
! local variables
!
      CHARACTER(LEN=1) :: trans

      CHARACTER(LEN=70) :: filenam

      INTEGER ::    i, &
                 ierr, &
                 incx, &
                 incy, &
                nvals, &
               islpsw, &
                 nmax

      REAL(KIND=rDef) :: alpha, &
                          beta

      REAL(KIND=rSP) ::      r, &
                         sigma, &
                          slp1, &
                          slp2, &
                         rDum

      REAL(KIND=rDef), DIMENSION(np) :: yy

      REAL(KIND=rSP), DIMENSION(np) :: yp, &
                                     temp

      REAL(KIND=rSP), DIMENSION(:), ALLOCATABLE :: xx, &
                                                  ymx, &
                                                  ymt, &
                                                  yab 
!
! Use fitpack to interpolate the given Mach distributions onto an
!  appropriate cosine mesh.
!
!     implicit real*8 (a-h,o-z)
!     parameter (NMAX = 128, NMAX4 = NMAX*4)
!     dimension rr(NMAX),rmx(NMAX),rmt(NMAX),snd(NMAX),yy(NMAX)
!
!     real*4 xx(NMAX),ymx(NMAX),ymt(NMAX),yab(NMAX),yp(NMAX)
!     real*4 temp(NMAX),slp1,slp2,sigma,r,curv2
!
!     dimension drm(NMAX),drt(NMAX),dsn(NMAX)
!     dimension dd(NMAX,NMAX)
!      dimension x(NMAX),y(NMAX),abr(NMAX),dbr(NMAX)
!     character*70 filenam
!     character*1  trans
!
      WRITE(6,*) 'Entering interp'
      trans = 'N'
      alpha = 1.0_rDef
      beta  = 0.0_rDef
      incx  = 1
      incy  = 1
!
      write(6,12)
 12   format(1x,'Input file name containing Mach distributions.')
      read(5,'(A70)') filenam
!
! Read in Mach values from data.
      nvals = 0
      open(unit=10,file=filenam,status='unknown')
      read(10,'(1x)')

   5  CONTINUE

!     do i = 1,NMAX
       read(10,*,end=100) rDum,rDum,rDum,rDum
       nvals = nvals +1
       GO TO 5
!     enddo
 100  continue

      REWIND(10)
      read(10,'(1x)')
      ALLOCATE(xx(nvals), &
              ymx(nvals), &
              ymt(nvals), &
              yab(nvals))

      DO i = 1,nvals
       read(10,*) xx(i),ymx(i),ymt(i),yab(i)
      END DO

      close(10)
      sig    =  REAL(xx(1),rDef)/REAL(xx(nvals),rDef)
      if (ifdff .eq. 0) then

         call grid(np  = np,  &
                   sig = sig, &
                   x   = yy,  &
                   r   = rr)

         call derivs(np  = np,  &
                     sig = sig, &
                     dl1 = dd,  &
                     ed2 = ed2, &
                     ed4 = ed4)
      else
         call fdgrid(np  = np,  &
                     sig = sig, &
                     x   = yy,  &
                     r   = rr)

         call fdrivs(np     = np,    &
                     sig    = sig,   &
                     dl1    = dd,    &
                     iorder = ifdff, &
                     ed2    = ed2,   &
                     ed4    = ed4)
      endif
!
! Call spline routines to fit distributions.
      slp1   =  0.0_rSP
      slp2   =  0.0_rSP
      islpsw =  3
      sigma  =  0.0_rSP
!
! Mx distribution.

      call curv1(nvals,xx,ymx,slp1,slp2,islpsw,yp,temp,sigma,ierr)

      if (ierr.ne.0) write(6,*) ierr

      do i = 1,np
       r      = REAL(rr(i),rSP)
       rmx(i) = REAL(curv2(r,nvals,xx,ymx,yp,sigma),rDef)
      enddo

      nmax = np

      call dgemv(&
          trans,& !Specifies the type of operation
          np   ,& 
          np   ,&
          alpha,&
          dd   ,&
          np   ,&
          rmx  ,&
          incx ,&
          beta ,&
          drm  ,&
          incy)
!
! Mt distribution.

      call curv1(nvals,xx,ymt,slp1,slp2,islpsw,yp,temp,sigma,ierr)
      if (ierr.ne.0) write(6,*) ierr

      do i = 1,np
       r      = REAL(rr(i),rSP)
       rmt(i) = REAL(curv2(r,nvals,xx,ymt,yp,sigma),rDef)
      enddo

      call dgemv(trans,np,np,alpha,dd,np,rmt,incx,beta,drt,incy)
!
! Abar distribution.
      call curv1(nvals,xx,yab,slp1,slp2,islpsw,yp,temp,sigma,ierr)
      if (ierr.ne.0) write(6,*) ierr
      do i = 1,np
       r      = REAL(rr(i),rSP)
       snd(i) = REAL(curv2(r,nvals,xx,yab,yp,sigma),rDef)
      enddo

      call dgemv(&
          trans,&
          np   ,&
          np   ,&
          alpha,&
          dd   ,&
          np   ,&
          snd  ,&
          incx ,&
          beta ,&
          dsn  ,&
          incy)
       
!
! Calculate isentropic Abar distribution based on input Mt distribution.
!$$$      gm    = 1.4
!$$$      do i = 1,np-1
!$$$       coeff = (1. -gm)/2.
!$$$       r     = rr(i)
!$$$       xlo   = r
!$$$       xup   = rr(np)
!$$$       n     = np -i +1
!$$$       do j = 1,np-i+1
!$$$        x(j) = rr(i+j-1)
!$$$        y(j) = rmt(i+j-1)*rmt(i+j-1)/rr(i+j-1)
!$$$       enddo
!$$$       call davint(x,y,n,xlo,xup,ans,ierr)
!$$$       abr(i) = exp(coeff*ans)
!$$$       write(6,*) i,xlo,xup,n,abr(i),ierr
!$$$      enddo
!$$$      call dgemv(trans,np,np,alpha,dd,NMAX,abr,incx,beta,dbr,incy)
!
      open(unit=12,file='spline.out',status='unknown')
      rewind 12
      write(12,15)
 15   format('#',7x,'rr',10x,'Mx',10x,'Mt',9x,'Abar',9x,'Ab_isen')
      do i = 1,np
!       write(12,20) rr(i),rmx(i),rmt(i),snd(i),abr(i)
        write(12,20) rr(i),rmx(i),rmt(i),snd(i)
      enddo
 20   format(5f12.6)
      close(12)
!
      DEALLOCATE(xx, &
                ymx, &
                ymt, &
                yab)

      WRITE(6,*) 'Leaving interp'
      return
      end
END MODULE interpModule
