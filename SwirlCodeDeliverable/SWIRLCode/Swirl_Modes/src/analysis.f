      subroutine analysis(np,np4,ak,rr,snd,rmx,rmt,aa,bb,alpha,beta,
     &   vl,vr,work,rwork,gamma,jobvl,jobvr,mm,ir,is,slp,vphi,akap)
c
      implicit real*8 (a-h,o-z)
      parameter (NMAX = 128, NMAX4 = NMAX*4)
c
      dimension rwork(8*np4)
      character jobvl,jobvr
      logical   col(500),row(500),badrow,badcol
c
      complex*16 alpha(NMAX4),beta(NMAX4),work(2*NMAX4),cvct(NMAX4)
      complex*16 ci,c0,ak
      complex*16 aa(NMAX4,NMAX4),bb(NMAX4,NMAX4),vphi(NMAX4)
      complex*16 vl(NMAX4,NMAX4),vr(NMAX4,NMAX4),gamma(NMAX4)
      dimension  rmx(NMAX),rmt(NMAX),rr(NMAX),snd(NMAX)
      dimension  akap(NMAX)
c
      ci      = (0.,1.)
      eps     = 1.e-8
c
c Compute convected wavenumbers.  Store them in a file.
      do j=1,np
       rm = rmx(j)
       rs = rmt(j)
       as = snd(j)
       r  = rr(j)
c       print*,'ak = ',ak,' as = ',as,' rm = ',rm
       if (rm.ne.0. .and. r.ne.0.) then
        cvct(j) = (ak/as -mm*rs/r)/rm
       endif
      enddo
      open(unit=22, 
     &     file='cv.waves.dat', 
     &     status='unknown')
      rewind 22
      do j=1,np
       write(22,19) cvct(j)
      enddo
      write(6,17) (cvct(j), j=1,np)
 17   format(1x,'Convected wavenumbers: ',/,8(f10.5))
 19   format(1x,2e15.5)
      close(22)
c
c Check for zero rows and columns in A.
      badcol = .false.
      do j=1,np4
       col(j) = .true.
       do k=1,np4
        if (abs(aa(k,j)).gt.eps) then
         col(j) = .false.
        endif
       enddo
      enddo
      do j=1,np4
       if (col(j)) then
        write(6,20) j
        badcol = .true.
       endif
      enddo
      badrow = .false.
      do k=1,np4
       row(k) = .true.
       do j=1,np4
        if (abs(aa(k,j)).gt.eps) then
         row(j) = .false.
        endif
       enddo
      enddo
      do k=1,np4
       if (row(k)) then
        write(6,25) k
        badrow = .true.
       endif
      enddo
c
      if (badrow.or.badcol) return
 20   format(1x,'Column ',i4,' contains all zeros.')
 25   format(1x,'Row    ',i4,' contains all zeros.')
c
      print *, jobvl, jobvr
c
      call ZGEGV(JOBVL,JOBVR,np4,aa,NMAX4,bb,NMAX4,ALPHA,BETA,
     $   VL,NMAX4,VR,NMAX4,WORK,2*NMAX4,RWORK,INFO )
c
      write(6,960) info
 960  format(1x,'info = ',i3)
c
      c0  = (0.,0.)
c
c Compute cut-off wavenumber for uniform flow.
      if (ir.eq.1 .and. slp.eq.0. .and. is.eq.0) then
       rm = rmx(1)
       gamco = real(ak)*rm/(rm*rm -1.)
       write(6,30) gamco
      endif
 30   format(/,1x,'Cut-off wavenumber: ',e15.5,/)
c
c Print the gammas to the display.
      write(6,500)
 500  format(1x)
      write(6,50)
      do j=1,np4
       if (beta(j).ne.c0) then
        gamma(j) = ci*alpha(j)/beta(j)
        if (abs(imag(gamma(j))).lt.eps) then
         gamma(j) = cmplx(real(gamma(j)),0.0d0)
        endif
        vphi(j)  = ak/gamma(j)
        write(6,10) j,gamma(j),gamma(j)/ak,vphi(j)
       endif
      enddo
 970  format(1x,i4,4e13.4)
c
c Print all the gammas to a file.
      open(unit=15,file='gammas.dat',status='unknown')
      open(unit=35,file='gam.acc',status='unknown')
      rewind 15
      rewind 35
      write(15,50)
      write(35,55)
 50   format(4x,'#',2x,'j',7x,'Re{gam}',7x,'Im{gam}',6x,'Re{gam}/k',
     &   6x,'Im{gam}/k',6x,'kappa')
 55   format(4x,'#',2x,'j',10x,'Re{gam}',13x,'Im{gam}',11x,'Re{gam/ak}',
     &   10x,'Im{gam/ak}',5x,'nz')
c
      do i = 1,np4
       if (ir.eq.1 .and. slp.eq.0. .and. is.eq.0) then
        rm   = rmx(1)
        akap(i) = (rm*rm -1.)*gamma(i)*gamma(i)
     &     -2.*real(ak)*rm*gamma(i) +real(ak)*real(ak)
        if (akap(i).gt.0.) then
         akap(i) = sqrt(akap(i))
         write(15,10) i,gamma(i),gamma(i)/ak,vphi(i),akap(i)
        else
         write(15,10) i,gamma(i),gamma(i)/ak,vphi(i)
        endif
       endif
       write(35,12) i,gamma(i),gamma(i)/ak,vphi(i)
       write(15,10) i,gamma(i),gamma(i)/ak,vphi(i)
      enddo
      close(15)
      close(35)
 10   format(1x,i4,9e15.6)
 12   format(1x,i4,6e20.12)
c
      return
      end
