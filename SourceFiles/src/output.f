      subroutine output(np,np4,mode,rho,omega,rmax,slp,ang,gam,
     &   egv,attenh,attend,rmx,drm,rmt,drt,snd,rr,wvn,vrm,vphi,is,icomp)
c
      implicit real*8 (a-h,o-z)
      parameter (NMAX = 128, NMAX4 = NMAX*4)
c
      parameter    (PI = 3.14159265358979324)
      dimension    rr(NMAX),izeros(1024),index(1024)
      dimension    rmx(NMAX),drm(NMAX),rmt(NMAX),drt(NMAX),snd(NMAX)
      dimension    nfile(32),mfile(32),akappa(NMAX)
      dimension    mu(NMAX),phi(NMAX)
      real*4       azeros(1024)
      complex*16   vrm(NMAX4,NMAX4),wvn(NMAX4),gamma,omega,cv,ci
      complex*16   vphi(NMAX4),attenh,attend,gam1(NMAX),gam2(NMAX)
      character    egv
      character*2  ff
      character*15 basen,basem
c
c Output files: 
c               output.data: has everything.
c               gam.nonconv: nonconvecting mode data.
c               gam.non.acc: nonconvecting mode data with more digits of accuracy.
c               gam.compare: compares the result from spectral and q3d methods.
c
      ci  = (0.,1.)
      eps = 1.e-4
c
c Output everything to an unformatted file.
      WRITE(6,*) 'omega = ',omega

      open(unit=12, 
     &     file='output.data', 
     &     form='unformatted')

      rewind(12)

      write(12) np,np4,mode,rho,omega,rmax,slp,ang,gam,attenh,attend
      write(12) (wvn(i), i=1,np4)
      write(12) (rr(i), i=1,np)
      write(12) ((vrm(i,j), i=1,np4), j=1,np4)
c      write(12) wvn
c      write(12) vrm
      close(12)
c
c Compute range of convected wavenumbers.
c
cdrh: np == number of radial points
c
      cvcmin =  1.e+4
      cvcmax = -1.e+4
      do i = 1,np
       r  =  rr(i)
       rx =  rmx(i)
       rt =  rmt(i)
       as =  snd(i)
       cv = (omega/as -mode/r*rt)/rx
       if (abs(cv).gt.cvcmax) cvcmax = abs(cv)
       if (abs(cv).lt.cvcmin) cvcmin = abs(cv)
      enddo
      if (cvcmin .ge. 0.) then
       cvcmin = cvcmin -eps
      else
       cvcmin = cvcmin +eps
      endif
      if (cvcmax .ge. 0.) then
       cvcmax = cvcmax +eps
      else
       cvcmax = cvcmax -eps
      endif
c
c Compute number of zero crossings for nonconvected modes.

      do i = 1,np4
       gamma = wvn(i)
       akx   = real(gamma)
       if (akx .le. cvcmin .or. akx .ge. cvcmax) then
          izeros(i) = 0
          aim    = dimag(vrm(3*np+1,i))
          are    =  real(vrm(3*np+1,i))
          phi(i) = atan2(aim,are)
          vold   = real(vrm(3*np+1,i)*exp(-ci*phi(i)))

          do j = 3*np+2,np4
             val = real(vrm(j,i)*exp(-ci*phi(i)))
             if (val*vold.lt.0.) then
                izeros(i) = izeros(i) +1
             endif
             vold = val
          enddo

          if (akx.eq.0.) izeros(i) = 100
       else
          izeros(i) = 200
       endif 
      enddo
c
c Sort modes by number of zero crossings.
      do i=1,np4
        azeros(i) = float(izeros(i))
      enddo
      call indexx(np4,azeros,index)
c
c Sort nonconvected modes into upstream and downstream.
      eps  = 1.e-3
      do j=1,np4
       if (izeros(index(j)).eq.izeros(index(j+1))) then
        gam1a = real(wvn(index(j)))
        gam2a = real(wvn(index(j+1)))
        if (gam2a .eq. 0.) then
           goto 1000
        endif
        if (abs(gam1a/gam2a -1.).lt.eps) then
         gim1 = imag(wvn(index(j)))
         gim2 = imag(wvn(index(j+1)))
         if (gim1.lt.gim2) then
          jtmp       = index(j)
          index(j)   = index(j+1)
          index(j+1) = jtmp
         endif
        else
           if (gam1a .eq. 0. .or. gam2a .eq. 0.) then
              print*, 'gam1a = ',gam1a,'  gam2a = ',gam2a
           else
              alm1 = 2.*PI/gam1a
              alm2 = 2.*PI/gam2a
           endif
         if (abs(alm1).lt.abs(alm2)) then
          jtmp       = index(j)
          index(j)   = index(j+1)
          index(j+1) = jtmp
         endif
        endif
       endif
      enddo
 1000 continue
c
c Eigenvector output.
c$$$      ncols = np4/15
c$$$      do n = 1,ncols+1
c$$$       write(ff,'(i2.2)') n
c$$$       nfile(n) = 25 +n
c$$$       mfile(n) = 55 +n
c$$$       basen    = 'postplot/egvre.'
c$$$       basem    = 'postplot/egvim.'
c$$$       open(unit=nfile(n),file=basen//ff,status='unknown')
c$$$       open(unit=mfile(n),file=basem//ff,status='unknown')
c$$$       rewind nfile(n)
c$$$       rewind mfile(n)
c$$$       write(nfile(n),5) np,mode,rho,omega,rmax,slp,ang,gam,
c$$$     &    attenh,attend
c$$$      enddo
c$$$ 5    format('#',2i4,9e12.5)
c$$$      if (egv .eq. 'V') then
c$$$       do j=1,np
c$$$        do n = 1,ncols+1
c$$$         nf  = nfile(n)
c$$$         mf  = mfile(n)
c$$$         mn  = (n -1)*15 +1
c$$$         mx  = n*15
c$$$         if (n.eq.ncols+1) then
c$$$          mx = min(mx,np4)
c$$$         endif
c$$$         write(nf,500) rr(j),(real(vrm(j+3*np,k)), k=mn,mx)
c$$$         write(mf,500) rr(j),(imag(vrm(j+3*np,k)), k=mn,mx)
c$$$        enddo
c$$$       enddo
c$$$      endif
c$$$ 500  format(16e12.3)
c$$$      do n = 1,ncols
c$$$       close(nfile(n))
c$$$       close(mfile(n))
c$$$      enddo
c
      open(unit=14,
     &     file='gam.nonconv',
     &     status='unknown')
      open(unit=16,
     &     file='gam.non.acc')
      rewind 14
      rewind 16
      write(14,40)
      write(16,42)
 40   format(4x,'#',2x,'j',6x,'Re{gam}',6x,'Im{gam}',4x,'Re{gam/ak}',
     &   3x,'Im{gam/ak}',2x,'nz')
 42   format(4x,'#',2x,'j',10x,'Re{gam}',13x,'Im{gam}',11x,'Re{gam/ak}',
     &   10x,'Im{gam/ak}',5x,'nz')
      do j=1,np4
       gamma = wvn(index(j))
       fac   = (1. +rho)/2.
       if (izeros(index(j)).lt.np-4) then
        write(14,10) index(j),gamma,gamma/omega,izeros(index(j))
        write(16,12) index(j),gamma,gamma/omega,izeros(index(j))
       endif
      enddo
 10   format(1x,i4,4e13.5,i4)
 12   format(1x,i4,4e20.12,i4)
      close(14)
      close(16)
c
      if (icomp .eq. 1) then
       open(unit=15,
     &      file='gam.compare',
     &      status='unknown')
       rewind 15
       write(15,35)
 35    format(4x,'i',18x,'gam_spec',32x,'gam_q3d',17x,'mu')
c
c Compute kappas.
       mumax = int(float(np)/PI)
       call kappa(mode,mumax,rho,mu,akappa)
c
c Compute average axial Mach number.
       sum = 0.
       do n = 2,np
        fn1 = rmx(n-1)
        fn  = rmx(n)
        sum = sum +(fn1 +fn)*(rr(n) -rr(n-1))
       enddo
       rmav = sum/(1. -rho)/2.
       rm   = rmav
       write(6,*) rmav
       do i = 1,mumax
        aki  = akappa(i)
        term = real(omega) -mode*ang
        disc = term*term +(rm*rm -1.)*aki*aki
        if (disc .ge. 0.) then
         gam1(i) = (rm*term +sqrt(disc))/(rm*rm -1.)
         gam2(i) = (rm*term -sqrt(disc))/(rm*rm -1.)
        else
         gam1(i) = (rm*term +ci*sqrt(-disc))/(rm*rm -1.)
         gam2(i) = (rm*term -ci*sqrt(-disc))/(rm*rm -1.)
        endif
       enddo
      endif
 25   format(1x,i4,4e20.12,i4)
      do i = 1,2*np
       if (izeros(index(i)).lt.mumax) then
        jj = (i +1)/2
        kk = i/2
        if (mod(i,2) .eq. 1) then
         write(15,25) izeros(index(i)),wvn(index(i)),gam2(jj)
        else
         write(15,25) izeros(index(i)),wvn(index(i)),gam1(kk),mu(kk)
        endif
       endif
      enddo
c
      return
      end
