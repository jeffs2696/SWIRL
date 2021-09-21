      subroutine rmach(npts,rr,rmch,drm,snd,dsn,dd,rxmax,slope,rro,ir)
c
      implicit real*8 (a-h,o-z)
      parameter (NMAX = 128)
      dimension rmch(NMAX),drm(NMAX),dd(NMAX,NMAX),rr(NMAX)
      dimension vrm(1024),vrmp(1024)
      dimension snd(NMAX),dsn(NMAX)
c
c  ir = 0 : uniform
c  ir = 1 : linear shear
c  ir = 2 : read shear distribution from a file
c  ir = 3 : uniform + sine wave boundary layers of thickness delta
c  ir = 4 : uniform + linear b.l.'s
c  ir = 5 : uniform + 1/7th power law b.l.'s
c  ir = 6 : hyperbolic secant profile, M(r) = M_0 sech(2/del*cosh^-1(2)r)
c  ir = 7 : laminar mean flow, M(r) = M_0 (1 -r^2)
c  ir = 8 : wavy sinusoid, M(r) = (M_max +M_min)/2 
c                           +(M_max -M_min)/2*sin(4*PI*R)
c  ir = 9 : Hagen-Poiseuille flow, Vx(r) = 1 +K ln r - r^2
c
      PI  = 3.1415926536
c
      if (ir.eq.0) then
       slope = 0.d0
       ir    = 1
      endif
      if (ir.eq.1) then
       do i = 1,npts
        r   = rr(i)
        if (slope.ge.0.) then
         vrm(i) = slope*(r -1.)  +rxmax
        else
         vrm(i) = slope*(r -rro) +rxmax
        endif 
        vrmp(i) = slope
       enddo
       do i = 1,npts
        drm(i)  = vrmp(i)/snd(i) -vrm(i)*dsn(i)/(snd(i)*snd(i))
        rmch(i) = vrm(i)/snd(i)
       enddo
      endif
      if (ir.eq.2) then
       open(unit=22,file='mach.input',status='unknown')
       read (22,*) (rmch(i), i = 1,npts)
       close(22)
c
c Spectral computation of M'.
       do k=1,npts
        sum = 0.
        do j=1,npts
         sum = sum +dd(k,j)*rmch(j)
        enddo
        drm(k) = sum
       enddo
      endif
      if (ir.eq.3) then
       delta     = slope
       do i = 1,npts
        r        =  rr(i)
        if ((r.le.(rro +delta)) .and. rro.ne.0.) then
         vrm(i)  =  rxmax*sin((r -rro)*PI/(2.*delta))
         vrmp(i) =  rxmax*PI/(2.*delta)*cos((r -rro)*PI/(2.*delta))
        elseif (r.lt.(1. -delta)) then
         vrm(i)  =  rxmax
         vrmp(i) =  0.
        else
         vrm(i)  =  rxmax*sin((1. -r)*PI/(2.*delta))
         vrmp(i) = -rxmax*PI/(2.*delta)*cos((1. -r)*PI/(2.*delta))
        endif
       enddo
       do i = 1,npts
        drm(i)  = vrmp(i)/snd(i) -vrm(i)*dsn(i)/(snd(i)*snd(i))
        rmch(i) = vrm(i)/snd(i)
       enddo
      endif
      if (ir.eq.4) then
       delta     = slope
       dels      = 2.*delta/(1. -rro)
       do i = 1,npts
        r        =  rr(i)
        reta     =  (2.*r -rro -1.)/(1. -rro)
        if (reta.ge.(-1. +dels) .and. reta.le.(1. -dels)) then
         vrm(i)  =  rxmax/(1. -0.5*dels)
         vrmp(i) =  0.
        elseif (reta.gt.(1. -dels)) then
         vrm(i)  =  rxmax*(1. -reta)/(dels*(1. -0.5*dels))
         vrmp(i) = -rxmax/(dels*(1. -0.5*dels))
        elseif ((reta.lt.(-1. +dels)).and. rro.ne.0.) then
         vrm(i)  =  rxmax*(1. +reta)/(dels*(1. -0.5*dels))
         vrmp(i) =  rxmax/(dels*(1. -0.5*dels))
        endif
       enddo
       do i = 1,npts
        drm(i)  = vrmp(i)/snd(i) -vrm(i)*dsn(i)/(snd(i)*snd(i))
        rmch(i) = vrm(i)/snd(i)
       enddo
      endif
      if (ir.eq.5) then
       rsi      = rro/(1. -rro)
       plsmns   = rsi +0.5
       do i = 2,npts-1
        r       = rr(i)
        rsh     = r/(1. -rro)
        if (rro.gt.0.) then
         pwl    = 1. -2.*abs(rsi +0.5 -rsh)
         vrm(i) = rxmax*pwl**(1./7.)
         if (rsh.le.plsmns) then
          sign  = -1
         else
          sign  =  1.
         endif
         vrmp(i) = -2.*rxmax/(7.*pwl**(6./7.))*sign
        else
         vrm(i)  =  rxmax*(1. -rsh)**(1./7.)
         vrmp(i) = -rxmax/7.*(1. -rsh)**(-6./7.)
        endif
       enddo
       if (rro.ne.0.) then
        vrm(1)  = 0.
        vrmp(1) = 1.1*vrmp(2)
cdrh: DRH addition: no value had been set at centerline of cylinder (r = 0)
       ELSE
        vrm(1)  = rxmax
        vrmp(1) = 0.
       endif
       vrm(npts)  = 0.
       vrmp(npts) = 1.1*vrmp(npts-1)
       do i = 1,npts
        drm(i)  = vrmp(i)/snd(i) -vrm(i)*dsn(i)/(snd(i)*snd(i))
        rmch(i) = vrm(i)/snd(i)
       enddo
      endif
      if (ir.eq.6) then
       acsh2 = log(2.0 +sqrt(2.*2. -1.))
       dels  = slope
       do i = 1,npts
        r       = rr(i)
        y       = 2.*(r -rro)/(1. -rro) -1.
        acnst   = 2.*acsh2/dels
        arg     = acnst*y
        rmch(i) = rxmax/cosh(arg)
        drm(i)  = -acnst*rxmax*tanh(arg)/cosh(arg)
       enddo
      endif
      if (ir.eq.9) then
       if (rro.ne.0.) then
        cnst = (rro*rro -1)/log(rro)
        dnom = 1. +cnst/2.*log(cnst/2.) -cnst/2.
        do i = 1,npts
         r       = rr(i)
         vrm(i)  = rxmax*(1. +cnst*log(r) -r*r)/dnom
         vrmp(i) = cnst/r -2.*r
        enddo
        do i = 1,npts
         drm(i)  = vrmp(i)/snd(i) -vrm(i)*dsn(i)/(snd(i)*snd(i))
         rmch(i) = vrm(i)/snd(i)
        enddo
       else
        ir = 7
       endif
      endif
      if (ir.eq.7) then
       den1   = 1./(rro -1.)
       den2   = 1./(1. -rro)
       acoeff = 4.*rxmax*den1*den2
       bcoeff = -acoeff*(1. +rro)
       ccoeff = acoeff*rro
       do i = 1,npts
        r       = rr(i)
        vrm(i)  = acoeff*r*r +bcoeff*r +ccoeff
        vrmp(i) = 2.*acoeff*r +bcoeff
       enddo
       do i = 1,npts
        drm(i)  = vrmp(i)/snd(i) -vrm(i)*dsn(i)/(snd(i)*snd(i))
        rmch(i) = vrm(i)/snd(i)
       enddo
      endif
      if (ir.eq.8) then
       pi2   = 2.*PI
       pi4   = 4.*PI
       rmax  = rxmax
       rmin  = slope
       acnst = rmax +rmin
       bcnst = rmax -rmin
       do i = 1,npts
        r = rr(i)
        y = 2.*(r -rro)/(1. -rro) -1.
        arg = pi4*y
        rmch(i) = 0.5*acnst +0.5*bcnst*sin(arg)
        drm(i)  = pi2*bcnst*cos(arg)
       enddo
      endif
c
      return
      end
