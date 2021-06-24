      subroutine smach(npts,rr,rmsw,rmswp,snd,dsn,dd,angom,gam,rro,is)
c
      implicit real*8 (a-h,o-z)
      parameter (NMAX = 128)
      dimension rmsw(NMAX),rmswp(NMAX),dd(NMAX,NMAX),rr(NMAX)
      dimension vsw(1024),vswp(1024)
      dimension snd(NMAX),dsn(NMAX)
c
c  is = 0: no swirl
c  is = 1: solid-body swirl; ang. vel. angom
c  is = 2: free-vortex swirl; gam/r
c  is = 3: solid-body and free-vortex swirl.
c  is = 4: Vt = 1/r^2; put in for stability tests.
c  is = 5: read in swirl from file.
c  is = 6: constant swirl across the duct; Vt = const.
c  is = 7: trailing line vortex; Vt = Vmax/r (1 - e^(-r^2))
c
      if (is.eq.0) then
       do i = 1,npts
        rmsw(i)  = 0.
        rmswp(i) = 0.
       enddo
      elseif (is.eq.1) then
       do i = 1,npts
        r        =  rr(i)
        vsw(i)   =  angom*r
        vswp(i)  =  angom
        rmsw(i)  =  vsw(i)/snd(i)
        rmswp(i) =  angom/snd(i)*(1. -r/snd(i)*dsn(i))
       enddo
      elseif (is.eq.2) then
       do i = 1,npts
        r        =  rr(i)
        vsw(i)   =  gam/r
        vswp(i)  = -gam/(r*r)
        rmsw(i)  =  vsw(i)/snd(i)
        rmswp(i) = -gam/(snd(i)*snd(i)*r*r)*(snd(i) +r*dsn(i))
       enddo
      elseif (is.eq.3) then
       do i = 1,npts
        r        =  rr(i)
        vsw(i)   =  angom*r +gam/r
        rmsw(i)  =  vsw(i)/snd(i)
        vswp(i)  =  angom -gam/(r*r)
        rmswp(i) =  angom/snd(i)*(1. -r/snd(i)*dsn(i))
     &     -gam/(snd(i)*snd(i)*r*r)*(snd(i) +r*dsn(i))
       enddo
      elseif (is.eq.4) then
       do i = 1,npts
        r        =  rr(i)
        vsw(i)   =  gam/(r*r)
        rmsw(i)  =  vsw(i)/snd(i)
        vswp(i)  = -2.*gam/(r*r*r)
        rmswp(i) = -gam/(r**3*snd(i)**2)*(2.*snd(i) +r*dsn(i))
       enddo
      elseif (is.eq.5) then
       open(unit=24,file='swrl.input',status='unknown')
       read (24,*) (rmsw(i), i = 1,npts)
       close(24)
c
c Spectral computation of M_theta'.
       do k=1,npts
        sum = 0.
        do j=1,npts
         sum = sum +dd(k,j)*rmsw(j)
        enddo
        rmswp(k) = sum
       enddo
      elseif (is.eq.6) then
       do i = 1,npts
        r        =  rr(i)
        vsw(i)   =  angom
        rmsw(i)  =  vsw(i)/snd(i)
        vswp(i)  =  0.
        rmswp(i) = -angom/snd(i)*dsn(i)
       enddo
      elseif (is.eq.7) then
       do i = 1,npts
        r        =  rr(i)
        vsw(i)   =  gam/r*(1. -exp(-r*r))
        rmsw(i)  =  vsw(i)/snd(i)
        vswp(i)  =  2.*gam*exp(-r*r) -gam/(r*r)*(1. -exp(r*r))
c
c Need to add rmswp calculation here.
       enddo
      endif
c
c Divide swirl velocity by speed of sound to get swirl Mach number.
c$$$      if (is.ne.5) then
c$$$       do i = 1,npts
c$$$        rmsw(i)  = vsw(i)/snd(i)
c$$$        rmswp(i) = vswp(i)/snd(i) -vsw(i)*dsn(i)/(snd(i)*snd(i))
c$$$       enddo
c$$$      endif
c
      return
      end
