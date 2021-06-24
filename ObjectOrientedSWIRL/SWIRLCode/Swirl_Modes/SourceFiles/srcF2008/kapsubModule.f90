MODULE kapsubModule
   USE, INTRINSIC :: ISO_FORTRAN_ENV
   IMPLICIT NONE
   PRIVATE
   PUBLIC :: kappa

INTERFACE kappa
  MODULE PROCEDURE kappa1
END INTERFACE kappa

   INTEGER, PARAMETER :: rDef = REAL64

CONTAINS

      subroutine kappa1(mm,mumax,sig,mu,akap)

      INTEGER, INTENT(IN) :: mm, &
                          mumax

      REAL(KIND=rDef), INTENT(IN) :: sig
      INTEGER, DIMENSION(:), INTENT(INOUT) :: mu
      REAL(KIND=rDef), DIMENSION(:), INTENT(INOUT) :: akap
!
! local variables
!
      INTEGER :: np, &
                np4, &
                  i, &
               kmax, &
               kpts, &
               mold

      REAL(KIND=rDef) :: pi, &
                      rxmax
      COMPLEX(KIND=rDef) :: c0, &
                            ak, &
                          etah, &
                          etad

      CHARACTER :: jobvl, &
                   jobvr

      REAL(KIND=rDef), DIMENSION(:), ALLOCATABLE :: rwork, &
                                                        r, &
                                                      rmx, &
                                                   akappa

      INTEGER, DIMENSION(:), ALLOCATABLE :: mmu
                                                       
      REAL(KIND=rDef), DIMENSION(:,:), ALLOCATABLE :: dl1

      COMPLEX(KIND=rDef), DIMENSION(:), ALLOCATABLE :: alpha, &
                                                        beta, &
                                                        work, &
                                                         wvn

      COMPLEX(KIND=rDef), DIMENSION(:,:), ALLOCATABLE :: aa, &
                                                         bb, &
                                                         vl, &
                                                         vr
!
!     implicit real*8 (a-h,o-z)
!     parameter (NMAX = 128, NMAX4 = NMAX*4)
!     parameter (PI = 3.141592653589793238462643)
!
!     dimension dl1(NMAX,NMAX),rwork(8*NMAX4),r(NMAX)
!     dimension rmx(NMAX),akap(NMAX),mu(NMAX)
!     dimension mmu(NMAX),akappa(NMAX)
!     character jobvl,jobvr
!
!     complex*16 alpha(NMAX4),beta(NMAX4),work(2*NMAX4)
!     complex*16 etah,etad,ak,c0
!     complex*16 aa(NMAX4,NMAX4),bb(NMAX4,NMAX4)
!     complex*16 vl(NMAX4,NMAX4),vr(NMAX4,NMAX4),wvn(NMAX4)
!
      pi    = 4.0_rDef*ATAN(1.0_rDef)
      c0    = CMPLX(0.0_rDef,0.0_rDef,rDef)

      np    = INT(REAL(mumax,rDef)*PI +1.0_rDef)
      np4   = 4*np

      ak    = CMPLX(10.0_rDef,0.0_rDef,rDef)
      rxmax = 0.5_rDef
      jobvl = 'N'
      jobvr = 'V'
      etah  = c0
      etad  = c0

      ALLOCATE(dl1(np,np),   &
               rwork(8*np4), &
               r(np),        &
               rmx(np),      &
               mmu(np),      &
               akappa(np4),  &
               alpha(np4),   &
               beta(np4),    &
               work(2*np4),  &
               aa(np4,np4),  &
               bb(np4,np4),  &
               vl(np4,np4),  &
               vr(np4,np4),  &
               wvn(np4))

!
      CALL ugrid(np  = np,  &
                 sig = sig, &
                 r   = r)

      CALL uderivs(np  = np,  &
                   sig = sig, &
                   dl1 = dl1)

      CALL urmach(npts  = np,  &
                  rr    = r,   &
                  rmch  = rmx, &
                  rxmax = rxmax)

      CALL uglobal(np   = np,  &
                   np4  = np4, &
                   sig  = sig, &
                   mode = mm,  &
                   om   = ak,  &
                   dd   = dl1, &
                   rr   = r,   &
                   rx   = rmx, &
                   aa   = aa,  &
                   bb   = bb)

      CALL uboundary(np   = np,   &
                     sig  = sig,  &
                     ak   = ak,   &
                     etah = etah, &
                     etad = etad, &
                     rmx  = rmx,  &
                     dd   = dl1,  &
                     aa   = aa,   &
                     bb   = bb)

      CALL uanalysis(np    = np,    &
                     np4   = np4,   &
                     ak    = ak,    &
                     rr    = r,     &
                     rmx   = rmx,   &
                     aa    = aa,    &
                     bb    = bb,    &
                     alpha = alpha, &
                     beta  = beta,  &
                     vl    = vl,    &
                     vr    = vr,    &
                     work  = work,  &
                     rwork = rwork, &
                     gam   = wvn,   &
                     jobvl = jobvl, &
                     jobvr = jobvr, &
                     mm    = mm)

      CALL uoutput(np     = np,    &
                   np4    = np4,   &
                   mode   = mm,    &
                   rho    = sig,   &
                   omega  = ak,    &
                   rmax   = rxmax, &
                   egv    = jobvr, &
                   attenh = etah,  &
                   attend = etad,  &
                   rmx    = rmx,   &
                   rr     = r,     &
                   wvn    = wvn,   &
                   vrm    = vr,    &
                   mu     = mmu,   &
                   aksort = akappa)
!
      kmax = 0

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
!
      DEALLOCATE(dl1,    &
                 rwork,  &
                 r,      &
                 rmx,    &
                 mmu,    &
                 akappa, &
                 alpha,  &
                 beta,   &
                 work,   &
                 aa,     &
                 bb,     &
                 vl,     &
                 vr,     &
                 wvn)
      return
      end

      subroutine uanalysis(np,np4,ak,rr,rmx,aa,bb,alpha,beta, &
         vl,vr,work,rwork,gam,jobvl,jobvr,mm)

      INTEGER, INTENT(IN) :: np, &
                            np4, &
                             mm

      COMPLEX(KIND=rDef), INTENT(IN) :: ak

      REAL(KIND=rDef), DIMENSION(:), INTENT(IN) :: rr, &
                                                  rmx
                              
      REAL(KIND=rDef), DIMENSION(:), INTENT(INOUT) :: rwork

      COMPLEX(KIND=rDef), DIMENSION(:), INTENT(INOUT) :: alpha, &
                                                          beta, &
                                                          work, &
                                                           gam

      COMPLEX(KIND=rDef), DIMENSION(:,:), INTENT(INOUT) :: aa, &
                                                           bb, &
                                                           vl, &
                                                           vr

      CHARACTER, INTENT(IN) :: jobvl, &
                               jobvr

! local variables

      INTEGER :: j, &
                 k, &
              info, &
             nmax4

      LOGICAL :: badcol, &
                 badrow

      LOGICAL, DIMENSION(np4) :: col, &
                                 row

      REAL(KIND=rDef) :: eps, &
                          rm

      COMPLEX(KIND=rDef) :: ci, &
                            c0

!
!     implicit real*8 (a-h,o-z)
!     parameter (NMAX = 128, NMAX4 = NMAX*4)
!
!     dimension rwork(8*np4)
!     character jobvl,jobvr
!     logical   col(500),row(500),badrow,badcol
!
!     complex*16 alpha(NMAX4),beta(NMAX4),work(2*NMAX4)
!     complex*16 ci,c0,ak
!     complex*16 aa(NMAX4,NMAX4),bb(NMAX4,NMAX4)
!     complex*16 vl(NMAX4,NMAX4),vr(NMAX4,NMAX4),gam(NMAX4)
!     dimension  rmx(NMAX),rr(NMAX)
!
      ci      = CMPLX(0.0_rDef,1.0_rDef,rDef)
      eps     = 1.e-8_rDef
!
! Compute convected wavenumbers.  Store them in a file.
      rm   = rmx(1)
!
! Check for zero rows and columns in A.
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
!
      if (badrow.or.badcol) return
 20   format(1x,'Column ',i4,' contains all zeros.')
 25   format(1x,'Row    ',i4,' contains all zeros.')
!
      nmax4 = np4

!     CALL ZGEGV(JOBVL,JOBVR,np4,aa,NMAX4,bb,NMAX4,ALPHA,BETA, &
!        VL,NMAX4,VR,NMAX4,WORK,2*NMAX4,RWORK,INFO )
!
!
! updated call
!
      CALL ZGGEV(JOBVL,   & ! JOBVL
                 JOBVR,   & ! JOBVR
                 np4,     & ! N
                 aa,      & ! A
                 NMAX4,   & ! LDA
                 bb,      & ! B
                 NMAX4,   & ! LDB
                 ALPHA,   & ! ALPHA
                 BETA,    & ! BETA
                 VL,      & ! VL
                 NMAX4,   & ! LDVL
                 VR,      & ! VR
                 NMAX4,   & ! LDVR
                 WORK,    & ! WORK
                 2*NMAX4, & ! LWORK
                 RWORK,   & ! RWORK
                 INFO )     ! INFO

!
      c0  = CMPLX(0.0_rDef,0.0_rDef,rDef)
!
! Print the gammas to the display.
      do j=1,np4
       if (beta(j).ne.c0) then
        gam(j) = ci*alpha(j)/beta(j)
        if (abs(aimag(gam(j))).lt.eps) then
         gam(j) = cmplx(real(gam(j)),0.0_rDef,rDef)
        endif
       endif
      enddo
!
!10   format(1x,i4,9e13.6)
      return
      WRITE(6,*) ak,mm,np,rr
      end

      subroutine uboundary(np,sig,ak,etah,etad,rmx,dd,aa,bb)

      INTEGER, INTENT(IN) :: np
      REAL(KIND=rDef), INTENT(IN) :: sig
      COMPLEX(KIND=rDef), INTENT(IN) :: ak, &
                                      etah, &
                                      etad

      REAL(KIND=rDef), DIMENSION(:), INTENT(IN) :: rmx
      REAL(KIND=rDef), DIMENSION(:,:), INTENT(IN) :: dd
      COMPLEX(KIND=rDef), DIMENSION(:,:), INTENT(INOUT) :: aa, &
                                                           bb

! local variables

      INTEGER :: j

      REAL(KIND=rDef) :: eps, &
                      etamag, &
                         rmh, &
                         rmd

      COMPLEX(KIND=rDef) :: ci
   
!
!     implicit real*8 (a-h,o-z)
!     parameter (NMAX = 128, NMAX4 = NMAX*4)
!
!     real*8     dd(NMAX,NMAX),rmx(NMAX),rmd,rmh
!     complex*16 aa(4*NMAX,4*NMAX),bb(4*NMAX,4*NMAX),etah,etad,ci,ak
!
! Boundary conditions.
      ci      = CMPLX(0.0_rDef,1.0_rDef,rDef)
      eps     = 1.e-4_rDef
!
      etamag = abs(etah) +abs(etad)
      if (etamag.gt.eps) then
       rmh    = rmx(1)
       rmd    = rmx(np)
       aa(np,np)   = ci*ak
       aa(np,2*np) = 0.0_rDef
       do j=1,np
        aa(np,3*np+j) = 0.0_rDef
       enddo
       aa(np,4*np) = -ci*ak*etad
       bb(np,np)   = 0.0_rDef
       bb(np,4*np) = etad*rmd
       if (sig.ne.0.0_rDef) then
        aa(1,1)    = -ci*ak
        aa(1,np+1) = 0.0_rDef
        do j=1,np
         aa(1,3*np+j) = 0.0_rDef
        enddo
        aa(1,3*np+1) = -ci*ak*etah
        bb(1,1)      = 0.0_rDef
        bb(1,3*np+1) = etah*rmh
       endif
      else
       if (sig.ne.0.0_rDef) then
        do j = 1,np
         aa(1,  np+j) = 0.0_rDef
         aa(1,3*np+j) = 0.0_rDef
         bb(1,     j) = 0.0_rDef
        enddo
       endif
       do j = 1,np
        aa(np,  np+j) = 0.0_rDef
        aa(np,3*np+j) = 0.0_rDef
        bb(np,     j) = 0.0_rDef
       enddo
      endif
!
      return
      WRITE(6,*) ak,dd
      end

      subroutine uderivs(np,sig,dl1)

      INTEGER, INTENT(IN) :: np
      REAL(KIND=rDef), INTENT(IN) :: sig
      REAL(KIND=rDef), DIMENSION(:,:), INTENT(INOUT) :: dl1
!
! local variables
!
      INTEGER :: n, &
                 i, &
                 j, &
                 k
      REAL(KIND=rDef) :: pi, &
                       pinr, &
                       pinh, &
                        cbk, &
                        cbj, &
                       xdif, &
                       sumj, &
                         s1, &
                         s2, &
                         xj
!
!     implicit real*8 (a-h,o-z)
!     parameter (NMAX = 128, NMAX4 = NMAX*4)
!     dimension  DL1(NMAX,NMAX)
!
!  COMPUTE ARRAY FOR CHEBYSHEV DIFFERENTIATION AT FACES.
!   See Canuto, p.69.
!
      n     = np -1
      pi    = 4.0_rDef*ATAN(1.0_rDef)
      PINR  = pi/REAL(n,rDef)
      PINH  = 0.5_rDef*PINr
!
      do k=1,np
       cbk  = 1.0_rDef
       if (k.eq.1 .or. k.eq.np) cbk = 2.0_rDef
       do j=1,np
        xj  = cos(REAL(j-1,rDef)*pinr)
        cbj = 1.0_rDef
        if (j.eq.1 .or. j.eq.np) cbj = 2.0_rDef
        if (j.ne.k) then
         xdif       = -2.0_rDef*sin(PINH*REAL(k+j-2,rDef))*sin(PINH*REAL(k-j,rDef))
         dl1(k,j)   = (cbk/cbj)*(-1.0_rDef)**(k +j)/xdif
        elseif (k.eq.1) then
         dl1(1,1)   =  (2.0_rDef*REAL(n*n,rDef) +1.0_rDef)/6.0_rDef
        elseif (k.eq.np) then
         dl1(np,np) = -(2.0_rDef*REAL(n*n,rDef) +1.0_rDef)/6.0_rDef
        else
         dl1(k,j)   = -xj/(2.0_rDef*sin(REAL(j-1,rDef)*PINR)**2)
        endif
       enddo
      enddo
!
! Fix derivative matrix so that rows sum to zero.
      do i = 1,np
       sumj = 0.0_rDef
       do j = 1,np
        if (i.ne.j) sumj = sumj +dl1(i,j)
       enddo
       dl1(i,i) = -sumj
      enddo
!
! Switch signs on 1st derivative matrix due to mesh orientation
!  from -1 to 1.
      s1      = 1.0_rDef -sig
      s2      = 2.0_rDef/s1
      do k=1,np
       do j=1,np
        dl1(k,j) = -dl1(k,j)*s2
       enddo
      enddo
!
      return
      end

      subroutine uglobal(np,np4,sig,mode,om,dd,rr,rx,aa,bb)

      INTEGER, INTENT(IN) :: np,  &
                             np4, &
                             mode

      REAL(KIND=rDef), INTENT(IN) :: sig

      REAL(KIND=rDef), DIMENSION(:,:), INTENT(IN) :: dd

      REAL(KIND=rDef), DIMENSION(:), INTENT(IN) :: rr,  &
                                                   rx

      COMPLEX(KIND=rDef), INTENT(IN) :: om

      COMPLEX(KIND=rDef), DIMENSION(:,:), INTENT(OUT) :: aa, &
                                                         bb

! define local variables

      INTEGER :: j,  &
                 k

      REAL(KIND=rDef) :: r

      COMPLEX(KIND=rDef) :: ci


!
!     implicit real*8 (a-h,o-z)
!     parameter (NMAX = 128, NMAX4 = NMAX*4)
!
!     dimension  dd(NMAX,NMAX),rx(NMAX),rr(NMAX)
!     real*8     r
!     complex*16 aa(NMAX4,NMAX4),bb(NMAX4,NMAX4),ci,om
!
      do j=1,np4
       do k=1,np4
        aa(k,j) = CMPLX(0.0_rDef,0.0_rDef,rDef)
        bb(k,j) = CMPLX(0.0_rDef,0.0_rDef,rDef)
       enddo
      enddo
!
      ci      = CMPLX(0.0_rDef,1.0_rDef,rDef)
!
! Global matrices.
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
         aa(  np+k,     j) = CMPLX(0.0_rDef,0.0_rDef,rDef)
         aa(2*np+k,     j) = CMPLX(0.0_rDef,0.0_rDef,rDef)
         if (r.ne.0.) then
          aa(  np+k,3*np+j) = ci*REAL(mode,rDef)/r
          aa(3*np+k,  np+j) = ci*REAL(mode,rDef)/r
          aa(3*np+k,     j) = aa(3*np+k,j) +1.0_rDef/r
         else
          aa(  np+k,3*np+j) = CMPLX(0.0_rDef,0.0_rDef,rDef)
          aa(3*np+k,  np+j) = CMPLX(0.0_rDef,0.0_rDef,rDef)
         endif
         bb(     k,     j) = rx(j)
         bb(  np+k,  np+j) = rx(j)
         bb(2*np+k,2*np+j) = rx(j)
         bb(3*np+k,3*np+j) = rx(j)
         bb(2*np+k,3*np+j) = CMPLX(1.0_rDef,0.0_rDef,rDef)
         bb(3*np+k,2*np+j) = CMPLX(1.0_rDef,0.0_rDef,rDef)
        endif
       enddo
      enddo
!
      return
      WRITE(6,*) sig     
      end

      subroutine ugrid(np,sig,r)

      INTEGER, INTENT(IN) :: np
      REAL(KIND=rDef), INTENT(IN) :: sig
      REAL(KIND=rDef), DIMENSION(:), INTENT(OUT) :: r
!
! local variables
!
      INTEGER :: j, &
                 n
  
      REAL(KIND=rDef) :: pi, &
                        pin, &
                     sigbar, &
                      coeff, &
                         xx
    
!
!  COMPUTE THE CHEBYSHEV GAUSS-LOBATTO GRID, reversed.
!    See Canuto, p. 67.
!
!     implicit real*8 (a-h,o-z)
!     parameter (NMAX = 128, NMAX4 = NMAX*4)
!     real*8 r(NMAX)
!
      PI     = 4.0_rDef*ATAN(1.0_rDef)
      n      = np -1
      PIN    = PI/REAL(N,rDef)
      sigbar = (1.0_rDef +sig)/(1.0_rDef -sig)
      coeff  = 0.5_rDef*(1.0_rDef -sig)
      DO j=1,NP
       xx    = -COS(REAL(j-1,rDef)*PIN)
       r(j)  = coeff*(xx +sigbar)
      END DO  
!
      return
      end

      SUBROUTINE uINDEXX(N,ARRIN,INDX)
      INTEGER, INTENT(IN) :: n
      REAL(KIND=rDef), DIMENSION(:), INTENT(IN) :: arrin
      INTEGER, DIMENSION(:), INTENT(INOUT) :: indx

! local variables

      INTEGER :: i, &
                 j, &
                 l, &
                ir, &
             indxt

      REAL(KIND=rDef) :: q

!     DIMENSION ARRIN(N),INDX(N)

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

      subroutine uoutput(np,np4,mode,rho,omega,rmax,egv,attenh,attend, &
         rmx,rr,wvn,vrm,mu,aksort)

      INTEGER, INTENT(IN) :: np, &
                            np4, &
                           mode
 
      INTEGER, DIMENSION(:), INTENT(INOUT) :: mu

      REAL(KIND=rDef), INTENT(IN) :: rho, &
                                    rmax

      REAL(KIND=rDef), DIMENSION(:), INTENT(IN) :: rmx, &
                                                    rr

      REAL(KIND=rDef), DIMENSION(:), INTENT(INOUT) :: aksort

      COMPLEX(KIND=rDef), INTENT(IN) :: omega, &
                                       attenh, &
                                       attend

      COMPLEX(KIND=rDef), DIMENSION(:), INTENT(IN) :: wvn

      COMPLEX(KIND=rDef), DIMENSION(:,:), INTENT(IN) :: vrm

      CHARACTER, INTENT(IN) :: egv
!
! local variables
!

      INTEGER :: i, &
                ii, &
                 j, &
              jtmp, &
             nkpts

      INTEGER, DIMENSION(np4) :: izeros, &
                                 indx

      REAL(KIND=rDef) :: rx, &
                         cv, &
                      cvmin, &
                      cvmax, &
                        akx, &
                        val, &
                       vold, &
                        eps, &
                       akap, &
                       alm1, &
                       alm2, &
                       gam1, &
                       gam2, &
                       gim1, &
                       gim2, &
                         pi

      REAL(KIND=rDef), DIMENSION(np4) :: aindex, &
                                         akappa

!
!     implicit real*8 (a-h,o-z)
!     parameter (NMAX = 128, NMAX4 = NMAX*4)
!
!     parameter   (PI = 3.14159265358979324)
!     dimension   rr(NMAX),izeros(1024),index(1024),kndex(1024)
!     dimension   rmx(NMAX),akappa(NMAX),aksort(NMAX),mu(NMAX)
!     dimension   nfile(32),mfile(32)
!     real*4      aindex(1024)
!     real*8      rx
!     complex*16  vrm(NMAX4,NMAX4),wvn(NMAX4),omega
!     complex*16  vphi(NMAX4),attenh,attend
!     character   egv
!     character*2 ff
!     character*6 basen,basem
!
! Compute convected wavenumber.

      pi = 4.0_rDef*ATAN(1.0_rDef)

      rx = rmx(1)
      cv = real(omega)/rx
      if (cv.gt.0.0_rDef) then
       cvmin = 0.9_rDef*cv
       cvmax = 1.1_rDef*cv
      else
       cvmin = 1.1_rDef*cv
       cvmax = 0.9_rDef*cv
      endif
!
! Compute kappas for acoustic modes.
      do i = 1,np4
       akx = real(wvn(i))
       if (akx.le.cvmin .or. akx.ge.cvmax) then
        akap = (rx*rx -1.0_rDef)*REAL(wvn(i))*REAL(wvn(i)) &
           -2.0_rDef*real(omega)*rx*REAL(wvn(i)) +real(omega)*real(omega)
        if (akap.ge.0.0_rDef) then
         akappa(i) = sqrt(akap)
         izeros(i) = 0
         vold      = real(vrm(3*np+1,i))
         do j = 3*np+2,np4
          val = real(vrm(j,i))
          if (val*vold.lt.0.0_rDef) then
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
!
! Sort modes by number of zero crossings.
      do i=1,np4
        aindex(i) = REAL(izeros(i),rDef)
      enddo

      CALL uindexx(n     = np4,    &
                   arrin = aindex, &
                   indx  = indx)
!
! Sort nonconvected modes into upstream and downstream.
      eps  = 1.e-3_rDef
!     do j=1,np4
      do j=1,np4-1
       if (izeros(indx(j)).eq.izeros(indx(j+1))) then
        gam1 = real(wvn(indx(j)))
        gam2 = real(wvn(indx(j+1)))
        if (abs(gam1/gam2 -1.0_rDef).lt.eps) then
         gim1 = aimag(wvn(indx(j)))
         gim2 = aimag(wvn(indx(j+1)))
         if (gim1.lt.gim2) then
          jtmp       = indx(j)
          indx(j)   = indx(j+1)
          indx(j+1) = jtmp
         endif
        else
         alm1 = 2.*PI/gam1
         alm2 = 2.*PI/gam2
         if (abs(alm1).lt.abs(alm2)) then
          jtmp       = indx(j)
          indx(j)   = indx(j+1)
          indx(j+1) = jtmp
         endif
        endif
       endif
      enddo
      nkpts = 0
      do i = 1,np4
       ii = indx(i)
       if (izeros(ii).ne.100) then
        if (abs(real(wvn(ii))).ge.eps) then
         nkpts = nkpts+1
         aksort(nkpts) = akappa(ii)
         mu(nkpts)     = izeros(ii)
        endif
       endif
      enddo
!
      return
      IF (ABS(attenh) > 0.0_rDef) CONTINUE
      IF (ABS(attend) > 0.0_rDef) CONTINUE
      WRITE(6,*) egv,mode,rho,rmax,rr
      end

      subroutine urmach(npts,rr,rmch,rxmax)

      INTEGER, INTENT(IN) :: npts
      REAL(KIND=rDef), DIMENSION(:), INTENT(IN) :: rr
      REAL(KIND=rDef), DIMENSION(:), INTENT(OUT) :: rmch
      REAL(KIND=rDef), INTENT(IN) :: rxmax

! local variables

      INTEGER :: i
!
!     implicit real*8 (a-h,o-z)
!     parameter (NMAX = 128, NMAX4 = NMAX*4)
!     dimension rmch(NMAX),rr(NMAX)
!
!  ir = 0 : uniform
!
      do i = 1,npts
       rmch(i) = rxmax
      enddo
!
      return
      IF (MAXVAL(rr) > 0.0_rDef) CONTINUE
      end

END MODULE kapsubModule
