      subroutine interp(np,sig,rr,rmx,drm,rmt,drt,snd,dsn,dd,
     $     ifdff,ed2,ed4)
c
c Use fitpack to interpolate the given Mach distributions onto an
c  appropriate cosine mesh.
c
      implicit real*8 (a-h,o-z)
      parameter (NMAX = 128, NMAX4 = NMAX*4)
      dimension rr(NMAX),rmx(NMAX),rmt(NMAX),snd(NMAX),yy(NMAX)
      real*4 xx(NMAX),ymx(NMAX),ymt(NMAX),yab(NMAX),yp(NMAX)
      real*4 temp(NMAX),slp1,slp2,sigma,r,curv2
      dimension drm(NMAX),drt(NMAX),dsn(NMAX)
      dimension dd(NMAX,NMAX)
c      dimension x(NMAX),y(NMAX),abr(NMAX),dbr(NMAX)
      character*70 filenam
      character*1  trans
c
      print*,'Entering interp'
      trans = 'N'
      alpha = 1.
      beta  = 0.
      incx  = 1
      incy  = 1
c
      write(6,12)
 12   format(1x,'Input file name containing Mach distributions.')
      read(5,'(A70)') filenam
c
c Read in Mach values from data.
      nvals = 0
      open(unit=10,file=filenam,status='unknown')
      read(10,'(1x)')
      do i = 1,NMAX
       read(10,*,end=100) xx(i),ymx(i),ymt(i),yab(i)
       nvals = nvals +1
      enddo
 100  continue
      close(10)
      sig    =  xx(1)/xx(nvals)
      if (ifdff .eq. 0) then
         call grid(np,sig,yy,rr)
         call derivs(np,sig,dd,ed2,ed4)
      else
         call fdgrid(np,sig,yy,rr)
         call fdrivs(np,sig,dd,ifdff,ed2,ed4)
      endif
c
c Call spline routines to fit distributions.
      slp1   =  0.
      slp2   =  0.
      islpsw =  3
      sigma  =  0.
c
c Mx distribution.
      call curv1(nvals,xx,ymx,slp1,slp2,islpsw,yp,temp,sigma,ierr)
      if (ierr.ne.0) write(6,*) ierr
      do i = 1,np
       r      = sngl(rr(i))
       rmx(i) = dble(curv2(r,nvals,xx,ymx,yp,sigma))
      enddo
      call dgemv(trans,np,np,alpha,dd,NMAX,rmx,incx,beta,drm,incy)
c
c Mt distribution.
      call curv1(nvals,xx,ymt,slp1,slp2,islpsw,yp,temp,sigma,ierr)
      if (ierr.ne.0) write(6,*) ierr
      do i = 1,np
       r      = sngl(rr(i))
       rmt(i) = dble(curv2(r,nvals,xx,ymt,yp,sigma))
      enddo
      call dgemv(trans,np,np,alpha,dd,NMAX,rmt,incx,beta,drt,incy)
c
c Abar distribution.
      call curv1(nvals,xx,yab,slp1,slp2,islpsw,yp,temp,sigma,ierr)
      if (ierr.ne.0) write(6,*) ierr
      do i = 1,np
       r      = sngl(rr(i))
       snd(i) = dble(curv2(r,nvals,xx,yab,yp,sigma))
      enddo
      call dgemv(trans,np,np,alpha,dd,NMAX,snd,incx,beta,dsn,incy)
c
c Calculate isentropic Abar distribution based on input Mt distribution.
c$$$      gm    = 1.4
c$$$      do i = 1,np-1
c$$$       coeff = (1. -gm)/2.
c$$$       r     = rr(i)
c$$$       xlo   = r
c$$$       xup   = rr(np)
c$$$       n     = np -i +1
c$$$       do j = 1,np-i+1
c$$$        x(j) = rr(i+j-1)
c$$$        y(j) = rmt(i+j-1)*rmt(i+j-1)/rr(i+j-1)
c$$$       enddo
c$$$       call davint(x,y,n,xlo,xup,ans,ierr)
c$$$       abr(i) = exp(coeff*ans)
c$$$       write(6,*) i,xlo,xup,n,abr(i),ierr
c$$$      enddo
c$$$      call dgemv(trans,np,np,alpha,dd,NMAX,abr,incx,beta,dbr,incy)
c
      open(unit=12,file='spline.out',status='unknown')
      rewind 12
      write(12,15)
 15   format('#',7x,'rr',10x,'Mx',10x,'Mt',9x,'Abar',9x,'Ab_isen')
      do i = 1,np
c       write(12,20) rr(i),rmx(i),rmt(i),snd(i),abr(i)
        write(12,20) rr(i),rmx(i),rmt(i),snd(i)
      enddo
 20   format(5f12.6)
      close(12)
c
      print*,'Leaving interp'
      return
      end
