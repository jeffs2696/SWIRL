MODULE analysisModule
    USE, INTRINSIC :: ISO_FORTRAN_ENV
    USE F90_ZGGEV 
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: analysis

    INTERFACE analysis
        MODULE PROCEDURE analysis1
    END INTERFACE

    INTEGER, PARAMETER :: rDef = REAL64

CONTAINS

    subroutine analysis1(np,np4,ak,rr,snd,rmx,rmt,aa,bb,alpha,beta, &
        vl,vr,work,rwork,gam,jobvl,jobvr,mm,ir,is,slp,vphi,akap)
!
!     implicit real*8 (a-h,o-z)
!     parameter (NMAX = 128, NMAX4 = NMAX*4)
!
!     dimension rwork(8*np4)
!     character jobvl,jobvr
!     logical   col(500),row(500),badrow,badcol
!
!     complex*16 alpha(NMAX4),beta(NMAX4),work(2*NMAX4),cvct(NMAX4)
!     complex*16 ci,c0,ak
!     complex*16 aa(NMAX4,NMAX4),bb(NMAX4,NMAX4),vphi(NMAX4)
!     complex*16 vl(NMAX4,NMAX4),vr(NMAX4,NMAX4),gam(NMAX4)
!     dimension  rmx(NMAX),rmt(NMAX),rr(NMAX),snd(NMAX)
!     dimension  akap(NMAX)
!

        INTEGER, INTENT(IN) :: np, &
            np4, &
            mm, &
            ir, &
            is

        REAL(KIND=rDef), INTENT(IN) :: slp

        REAL(KIND=rDef), DIMENSION(:), INTENT(IN) :: rr, &
            snd, &
            rmx, &
            rmt, &
            rwork

        REAL(KIND=rDef), DIMENSION(:), INTENT(OUT) :: akap

        COMPLEX(KIND=rDef), INTENT(IN) :: ak

        COMPLEX(KIND=rDef), DIMENSION(:), INTENT(IN) :: alpha, &
            beta, &
            work

        COMPLEX(KIND=rDef), DIMENSION(:), INTENT(OUT) :: gam, &
            vphi

        COMPLEX(KIND=rDef), DIMENSION(:,:), INTENT(IN) :: aa, &
            bb, &
            vl, &
            vr

        CHARACTER, INTENT(IN) :: jobvl, &
            jobvr

! define local variables

        LOGICAL :: badcol, &
            badrow

        LOGICAL, DIMENSION(np4) :: col, &
            row

        COMPLEX(KIND=rDef) :: c0, &
            ci, &
            beta_non_zero

        COMPLEX(KIND=rDef), DIMENSION(np4) :: cvct

        INTEGER :: i, &
            j, &
            k, &
            info, &
            nmax4

        REAL(KIND=rDef) :: as, &
            eps, &
            ! gamco, &
            r, &
            rm, &
            rs

        ci      = CMPLX(0.0_rDef,1.0_rDef,rDef)
        eps     = 1.e-8_rDef
!
! Compute convected wavenumbers.  Store them in a file.
        do j=1,np

            rm = rmx(j)
            rs = rmt(j)
            as = snd(j)
            r  = rr(j)

!       print*,'ak = ',ak,' as = ',as,' rm = ',rm

            ! if ( (rm.ne.0.0_rDef) .and. (r.ne.0.0_rDef) ) then

                cvct(j) = (ak/CMPLX(as,KIND=rDef) -CMPLX(mm,KIND=rDef)*CMPLX(rs,KIND=rDef)/CMPLX(r,KIND=rDef))/CMPLX(rm,KIND=rDef)


            ! endif

        enddo

        open(unit=22,               &
            file='cv.waves.dat',   &
            status='unknown')
        rewind 22

        do j=1,np
            write(22,19) cvct(j)
        enddo

        ! write(6,17) (cvct(j), j=1,np)
! 17      format(1x,'Convected wavenumbers: ',/,8(f10.5))

19      format(1x,2e15.5)

        close(22)

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
                ! write(6,20) j
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
                ! write(6,25) k
                badrow = .true.
            endif

        enddo
!
        if (badrow.or.badcol) return
! 20      format(1x,'Column ',i4,' contains all zeros.')
! 25      format(1x,'Row    ',i4,' contains all zeros.')
!
        print *, jobvl, jobvr
!
!     CALL ZGEGV(JOBVL,JOBVR,np4,aa,NMAX4,bb,NMAX4,ALPHA,BETA, &
!                VL,NMAX4,VR,NMAX4,WORK,2*NMAX4,RWORK,INFO )

        nmax4 = np4

!     CALL ZGEGV(JOBVL,JOBVR,np4,aa,NMAX4,bb,NMAX4,ALPHA,BETA, &
!                VL,NMAX4,VR,NMAX4,WORK,2*NMAX4,RWORK,INFO )

! updated call

            CALL USE_EIGENSOLVER(&
            JOBVL = JOBVL  ,   & ! JOBVL
            JOBVR = JOBVR  ,   & ! JOBVR
            N     = np4      ,     & ! N
            A     = aa,      & ! A
            LDA   = NMAX4,   & ! LDA
            B     = bb,      & ! B
            LDB   = NMAX4,   & ! LDB
            ALPHA = ALPHA,   & ! ALPHA
            BETA  = BETA,    & ! BETA
            VL    = VL,      & ! VL
            LDVL  = NMAX4,   & ! LDVL
            VR    = VR,      & ! VR
            LDVR  = NMAX4,   & ! LDVR
            WORK  = WORK,    & ! WORK
            LWORK = 2*NMAX4, & ! LWORK
            RWORK = RWORK,   & ! RWORK
            INFO  = INFO )     ! INFO

!        CALL ZGGEV(&
!            JOBVL,   & ! JOBVL
!            JOBVR,   & ! JOBVR
!            np4,     & ! N
!            aa,      & ! A
!            NMAX4,   & ! LDA
!            bb,      & ! B
!            NMAX4,   & ! LDB
!            ALPHA,   & ! ALPHA
!            BETA,    & ! BETA
!            VL,      & ! VL
!            NMAX4,   & ! LDVL
!            VR,      & ! VR
!            NMAX4,   & ! LDVR
!            WORK,    & ! WORK
!            2*NMAX4, & ! LWORK
!            RWORK,   & ! RWORK
!            INFO )     ! INFO
!
        ! write(6,960) info
! 960     format(1x,'info = ',i3)
!
        c0  = CMPLX(0.0_rDef,0.0_rDef,rDef)
!
! Compute cut-off wavenumber for uniform flow.
! getting -Werror=compare-reals error with the if statements

      !  if ((ir.eq.1) .and. (slp.eq.0.0_rDef) .and. (is.eq.0)) then
      !      rm = rmx(1)
      !      gamco = REAL(ak,rDef)*rm/(rm*rm -1.0_rDef)
      !      write(6,30) gamco
      !  endif
! 30      format(/,1x,'Cut-off wavenumber: ',e15.5,/)
!
! Print the gammas to the display.
        ! write(6,500)
! 500     format(1x)
        ! write(6,50)
        do j=1,np4

        beta_non_zero = beta(j)
            if (beta_non_zero.ne.c0) then
                gam(j) = ci*alpha(j)/beta(j)
                if (abs(AIMAG(gam(j))).lt.eps) then
                    gam(j) = CMPLX(REAL(gam(j)),0.0d0,rDef)
                endif
                vphi(j)  = ak/gam(j)
                ! write(6,10) j,gam(j),gam(j)/ak,vphi(j)
            endif
        enddo
!970  format(1x,i4,4e13.4)
!
! Print all the gammas to a file.
        open(unit=15,file='gammas.dat',status='unknown')
        open(unit=35,file='gam.acc',status='unknown')
        open(unit=55,file='gammasOnly.dat',status='unknown')
        rewind 15
        rewind 35
        rewind 55
        write(15,50)
        write(35,55)
50      format('#',3x,'j',7x,'Re{gam}',7x,'Im{gam}',6x,'Re{gam}/k', &
            6x,'Im{gam}/k',6x,'kappa')
55      format('#',3x,'j',10x,'Re{gam}',13x,'Im{gam}',11x,'Re{gam/ak}', &
            10x,'Im{gam/ak}',5x,'nz')
!
        do i = 1,np4
! JS: if there is (linear shear) and (no slope) and (no swirl then) ...
! Note: this will never happen because we removed the swrl.input functionality 
            if ((ir.eq.1) .and. (slp.eq.0.0_rDef) .and. (is.eq.0)) then
                rm   = rmx(1)
!       akap(i) = (rm*rm -1.)*gam(i)*gam(i) &
!          -2.*real(ak)*rm*gam(i) +real(ak)*real(ak)
                akap(i) = (rm*rm -1.0_rDef)*REAL(gam(i)*gam(i),rDef) &
                    -2.0_rDef*REAL(ak,rDef)*rm*REAL(gam(i),rDef)      &
                    +REAL(ak,rDef)*REAL(ak,rDef)
                if (akap(i).gt.0.0_rDef) then
                    akap(i) = SQRT(akap(i))
                    write(15,10) i,gam(i),gam(i)/ak,vphi(i),akap(i)
                else
                    write(15,10) i,gam(i),gam(i)/ak,vphi(i)
                endif
! JS: if there is not linear shear and there is no swirl flag then proceed
            endif
! because gam has blank entries (gives 1e-310 because previous loop omitted some entries)
            ! write(35,12) i,gam(i),gam(i)/ak,vphi(i)
            ! write(15,10) i,gam(i),gam(i)/ak,vphi(i)
            ! write(55,*) REAL(gam(i)),AIMAG(gam(i))
        enddo
        close(15)
        close(35)
        close(55)
10      format(1x,i4,9e15.6)
! 12      format(1x,i4,6e20.12)
!
        return
    end

END MODULE analysisModule
