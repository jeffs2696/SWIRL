      subroutine kappa(mm,mumax,sig,mu,akap)
c
      implicit real*8 (a-h,o-z)
      parameter (NMAX = 128, NMAX4 = NMAX*4)
      parameter (PI = 3.141592653589793238462643)
c
      dimension dl1(NMAX,NMAX),rwork(8*NMAX4),r(NMAX)
      dimension rmx(NMAX),akap(NMAX),mu(NMAX)
      dimension mmu(NMAX),akappa(NMAX)
      character jobvl,jobvr
c
      complex*16 alpha(NMAX4),beta(NMAX4),work(2*NMAX4)
      complex*16 etah,etad,ak,c0
      complex*16 aa(NMAX4,NMAX4),bb(NMAX4,NMAX4)
      complex*16 vl(NMAX4,NMAX4),vr(NMAX4,NMAX4),wvn(NMAX4)
c
      c0    = (0.,0.)
      np    = int(float(mumax)*PI +1)
      np4   = 4*np
      ak    = dcmplx(10.d0,0.0d0)
      rxmax = 0.5
      jobvl = 'N'
      jobvr = 'V'
      etah  = c0
      etad  = c0
c
      call ugrid(np,sig,r)
      call uderivs(np,sig,dl1)
      call urmach(np,r,rmx,rxmax)
      call uglobal(np,np4,sig,mm,ak,dl1,r,rmx,aa,bb)
      call uboundary(np,sig,ak,etah,etad,rmx,dl1,aa,bb)
      call uanalysis(np,np4,ak,r,rmx,aa,bb,alpha,beta,vl,vr,work,rwork,
     &   wvn,jobvl,jobvr,mm)
      call uoutput(np,np4,mm,sig,ak,rxmax,jobvr,etah,etad,rmx,r,
     &   wvn,vr,mmu,akappa)
c
      do i = 1,np
       if (mmu(i) .le. mumax) then
        kmax = i
       endif
      enddo
      mold = 100
      kpts = 0
      do i = 1,kmax
       if (mmu(i) .ne. mold) then
        kpts       = kpts +1
        akap(kpts) = akappa(i)
        mu(kpts)   = mmu(i)
        mold       = mmu(i)
       endif
      enddo
c
      return
      end
      subroutine uanalysis(np,np4,ak,rr,rmx,aa,bb,alpha,beta,
     &   vl,vr,work,rwork,gamma,jobvl,jobvr,mm)
c
      implicit real*8 (a-h,o-z)
      parameter (NMAX = 128, NMAX4 = NMAX*4)
c
      dimension rwork(8*np4)
      character jobvl,jobvr
      logical   col(500),row(500),badrow,badcol
c
      complex*16 alpha(NMAX4),beta(NMAX4),work(2*NMAX4)
      complex*16 ci,c0,ak
      complex*16 aa(NMAX4,NMAX4),bb(NMAX4,NMAX4)
      complex*16 vl(NMAX4,NMAX4),vr(NMAX4,NMAX4),gamma(NMAX4)
      dimension  rmx(NMAX),rr(NMAX)
c
      ci      = (0.,1.)
      eps     = 1.e-8
c
c Compute convected wavenumbers.  Store them in a file.
      rm   = rmx(1)
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
      call ZGEGV(JOBVL,JOBVR,np4,aa,NMAX4,bb,NMAX4,ALPHA,BETA,
     $   VL,NMAX4,VR,NMAX4,WORK,2*NMAX4,RWORK,INFO )
c
      c0  = (0.,0.)
c
c Print the gammas to the display.
      do j=1,np4
       if (beta(j).ne.c0) then
        gamma(j) = ci*alpha(j)/beta(j)
        if (abs(imag(gamma(j))).lt.eps) then
         gamma(j) = cmplx(real(gamma(j)),0.0d0)
        endif
       endif
      enddo
c
 10   format(1x,i4,9e13.6)
      return
      end
      subroutine uboundary(np,sig,ak,etah,etad,rmx,dd,aa,bb)
c
      implicit real*8 (a-h,o-z)
      parameter (NMAX = 128, NMAX4 = NMAX*4)
c
      real*8     dd(NMAX,NMAX),rmx(NMAX),rmd,rmh
      complex*16 aa(4*NMAX,4*NMAX),bb(4*NMAX,4*NMAX),etah,etad,ci,ak
c
c Boundary conditions.
      ci      = (0.,1.)
      eps     = 1.e-4
c
      etamag = abs(etah) +abs(etad)
      if (etamag.gt.eps) then
       rmh    = rmx(1)
       rmd    = rmx(np)
       aa(np,np)   = ci*ak
       aa(np,2*np) = 0.
       do j=1,np
        aa(np,3*np+j) = 0.
       enddo
       aa(np,4*np) = -ci*ak*etad
       bb(np,np)   = 0.
       bb(np,4*np) = etad*rmd
       if (sig.ne.0.) then
        aa(1,1)    = -ci*ak
        aa(1,np+1) = 0.
        do j=1,np
         aa(1,3*np+j) = 0.
        enddo
        aa(1,3*np+1) = -ci*ak*etah
        bb(1,1)      = 0.
        bb(1,3*np+1) = etah*rmh
       endif
      else
       if (sig.ne.0.) then
        do j = 1,np
         aa(1,  np+j) = 0.
         aa(1,3*np+j) = 0.
         bb(1,     j) = 0.
        enddo
       endif
       do j = 1,np
        aa(np,  np+j) = 0.
        aa(np,3*np+j) = 0.
        bb(np,     j) = 0.
       enddo
      endif
c
      return
      end
      subroutine uderivs(np,sig,dl1)
c
      implicit real*8 (a-h,o-z)
      parameter (NMAX = 128, NMAX4 = NMAX*4)
      dimension  DL1(NMAX,NMAX)
c
C  COMPUTE ARRAY FOR CHEBYSHEV DIFFERENTIATION AT FACES.
c   See Canuto, p.69.
c
      n     = np -1
      PINR  = 3.1415926535898/N 
      PINH  = 0.5*PINr
c
      do k=1,np
       cbk  = 1.0
       if (k.eq.1 .or. k.eq.np) cbk = 2.0
       do j=1,np
        xj  = cos((j -1)*pinr)
        cbj = 1.0
        if (j.eq.1 .or. j.eq.np) cbj = 2.0
        if (j.ne.k) then
         xdif       = -2.*sin(PINH*(k +j -2))*sin(PINH*(k -j))
         dl1(k,j)   = (cbk/cbj)*(-1.0)**(k +j)/xdif
        elseif (k.eq.1) then
         dl1(1,1)   =  (2.*n*n +1.)/6.
        elseif (k.eq.np) then
         dl1(np,np) = -(2.*n*n +1.)/6.
        else
         dl1(k,j)   = -xj/(2.*sin((j -1)*PINR)**2)
        endif
       enddo
      enddo
c
c Fix derivative matrix so that rows sum to zero.
      do i = 1,np
       sumj = 0.
       do j = 1,np
        if (i.ne.j) sumj = sumj +dl1(i,j)
       enddo
       dl1(i,i) = -sumj
      enddo
c
c Switch signs on 1st derivative matrix due to mesh orientation
c  from -1 to 1.
      s1      = 1. -sig
      s2      = 2./s1
      do k=1,np
       do j=1,np
        dl1(k,j) = -dl1(k,j)*s2
       enddo
      enddo
c
      return
      end
      subroutine uglobal(np,np4,sig,mode,om,dd,rr,rx,aa,bb)
c
      implicit real*8 (a-h,o-z)
      parameter (NMAX = 128, NMAX4 = NMAX*4)
c
      dimension  dd(NMAX,NMAX),rx(NMAX),rr(NMAX)
      real*8     r
      complex*16 aa(NMAX4,NMAX4),bb(NMAX4,NMAX4),ci,om
c
      do j=1,np4
       do k=1,np4
        aa(k,j) = 0.
        bb(k,j) = 0.
       enddo
      enddo
c
      ci      = (0.,1.)
c
c Global matrices.
      do k=1,np
       r   = rr(k)
       do j=1,np
        aa(     k,3*np+j) = dd(k,j)
        aa(3*np+k,     j) = dd(k,j)
        if (k.eq.j) then
         aa(     k,     j) = -ci*om
         aa(  np+k,  np+j) = -ci*om
         aa(2*np+k,2*np+j) = -ci*om
         aa(3*np+k,3*np+j) = -ci*om
         aa(  np+k,     j) = 0.d0
         aa(2*np+k,     j) = 0.d0
         if (r.ne.0.) then
          aa(  np+k,3*np+j) = ci*float(mode)/r
          aa(3*np+k,  np+j) = ci*float(mode)/r
          aa(3*np+k,     j) = aa(3*np+k,j) +1.d0/r
         else
          aa(  np+k,3*np+j) = 0.d0
          aa(3*np+k,  np+j) = 0.d0
         endif
         bb(     k,     j) = rx(j)
         bb(  np+k,  np+j) = rx(j)
         bb(2*np+k,2*np+j) = rx(j)
         bb(3*np+k,3*np+j) = rx(j)
         bb(2*np+k,3*np+j) = 1.d0
         bb(3*np+k,2*np+j) = 1.d0
        endif
       enddo
      enddo
c
      return
      end

      subroutine ugrid(np,sig,r)
c
C  COMPUTE THE CHEBYSHEV GAUSS-LOBATTO GRID, reversed.
c    See Canuto, p. 67.
c
      implicit real*8 (a-h,o-z)
      parameter (NMAX = 128, NMAX4 = NMAX*4)
      real*8 r(NMAX)
c
      PI     = 3.14159265358979324
      n      = np -1
      PIN    = PI/N
      sigbar = (1. +sig)/(1. -sig)
      coeff  = 0.5*(1. -sig)
      DO 10 j=1,NP
       xx    = -COS((j-1)*PIN)
       r(j)  = coeff*(xx +sigbar)
 10   CONTINUE
c
      return
      end

      SUBROUTINE uINDEXX(N,ARRIN,INDX)
      DIMENSION ARRIN(N),INDX(N)
      DO 11 J=1,N
        INDX(J)=J
11    CONTINUE
      IF(N.EQ.1)RETURN
      L=N/2+1
      IR=N
10    CONTINUE
        IF(L.GT.1)THEN
          L=L-1
          INDXT=INDX(L)
          Q=ARRIN(INDXT)
        ELSE
          INDXT=INDX(IR)
          Q=ARRIN(INDXT)
          INDX(IR)=INDX(1)
          IR=IR-1
          IF(IR.EQ.1)THEN
            INDX(1)=INDXT
            RETURN
          ENDIF
        ENDIF
        I=L
        J=L+L
20      IF(J.LE.IR)THEN
          IF(J.LT.IR)THEN
            IF(ARRIN(INDX(J)).LT.ARRIN(INDX(J+1)))J=J+1
          ENDIF
          IF(Q.LT.ARRIN(INDX(J)))THEN
            INDX(I)=INDX(J)
            I=J
            J=J+J
          ELSE
            J=IR+1
          ENDIF
        GO TO 20
        ENDIF
        INDX(I)=INDXT
      GO TO 10
      END
      subroutine uoutput(np,np4,mode,rho,omega,rmax,egv,attenh,attend,
     &   rmx,rr,wvn,vrm,mu,aksort)
c
      implicit real*8 (a-h,o-z)
      parameter (NMAX = 128, NMAX4 = NMAX*4)
c
      parameter   (PI = 3.14159265358979324)
      dimension   rr(NMAX),izeros(1024),index(1024),kndex(1024)
      dimension   rmx(NMAX),akappa(NMAX),aksort(NMAX),mu(NMAX)
      dimension   nfile(32),mfile(32)
      real*4      aindex(1024)
      real*8      rx
      complex*16  vrm(NMAX4,NMAX4),wvn(NMAX4),omega
      complex*16  vphi(NMAX4),attenh,attend
      character   egv
      character*2 ff
      character*6 basen,basem
c
c Compute convected wavenumber.
      rx = rmx(1)
      cv = real(omega)/rx
      if (cv.gt.0.) then
       cvmin = 0.9*cv
       cvmax = 1.1*cv
      else
       cvmin = 1.1*cv
       cvmax = 0.9*cv
      endif
c
c Compute kappas for acoustic modes.
      do i = 1,np4
       akx = real(wvn(i))
       if (akx.le.cvmin .or. akx.ge.cvmax) then
        akap = (rx*rx -1.d0)*wvn(i)*wvn(i)
     &     -2.d0*real(omega)*rx*wvn(i) +real(omega)*real(omega)
        if (akap.ge.0.) then
         akappa(i) = sqrt(akap)
         izeros(i) = 0
         vold      = real(vrm(3*np+1,i))
         do j = 3*np+2,np4
          val = real(vrm(j,i))
          if (val*vold.lt.0.) then
           izeros(i) = izeros(i) +1
          endif
          vold = val
         enddo
        else
         izeros(i) = 100
        endif
       else
        izeros(i) = 100
       endif
      enddo
c
c Sort modes by number of zero crossings.
      do i=1,np4
        aindex(i) = float(izeros(i))
      enddo
      call uindexx(np4,aindex,index)
c
c Sort nonconvected modes into upstream and downstream.
      eps  = 1.e-3
      do j=1,np4
       if (izeros(index(j)).eq.izeros(index(j+1))) then
        gam1 = real(wvn(index(j)))
        gam2 = real(wvn(index(j+1)))
        if (abs(gam1/gam2 -1.).lt.eps) then
         gim1 = imag(wvn(index(j)))
         gim2 = imag(wvn(index(j+1)))
         if (gim1.lt.gim2) then
          jtmp       = index(j)
          index(j)   = index(j+1)
          index(j+1) = jtmp
         endif
        else
         alm1 = 2.*PI/gam1
         alm2 = 2.*PI/gam2
         if (abs(alm1).lt.abs(alm2)) then
          jtmp       = index(j)
          index(j)   = index(j+1)
          index(j+1) = jtmp
         endif
        endif
       endif
      enddo
      nkpts = 0
      do i = 1,np4
       ii = index(i)
       if (izeros(ii).ne.100) then
        if (abs(real(wvn(ii))).ge.eps) then
         nkpts = nkpts +1
         aksort(nkpts) = akappa(ii)
         mu(nkpts)     = izeros(ii)
        endif
       endif
      enddo
c
      return
      end
      subroutine urmach(npts,rr,rmch,rxmax)
c
      implicit real*8 (a-h,o-z)
      parameter (NMAX = 128, NMAX4 = NMAX*4)
      dimension rmch(NMAX),rr(NMAX)
c
c  ir = 0 : uniform
c
      do i = 1,npts
       rmch(i) = rxmax
      enddo
c
      return
      end
