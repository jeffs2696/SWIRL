! ANALYSISMODULE.f90 - Utilizes ZGGEV to calculate the eigenvalues and vectors
! of Equation 2.51 in the SwirlCodePaper. The eigenvalues are then sorted but
! the current equation used to solve for them is unclear.
!
! ZGGEV provides the eigenvalues in the form of lambda = alpha/beta where 
! sometimes beta is zero. This module returns "gam", which is = alpha/beta for 
! a user defined zero. It is currently set to double precision.
MODULE analysisModule
    USE, INTRINSIC :: ISO_FORTRAN_ENV
    USE IEEE_ARITHMETIC
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: analysis

    INTERFACE analysis
        MODULE PROCEDURE analysis1
    END INTERFACE

    INTEGER, PARAMETER :: rDef = REAL64

CONTAINS

    subroutine analysis1(np,np4,ak,rr,snd,rmx,rmt,aa,bb,alpha,beta, &
            vl,vr,work,rwork,gam,jobvl,jobvr,mm,ir,is,slp,vphi,akap,S_MMS)


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
        COMPLEX(KIND=rDef) ,DIMENSION(:,:), INTENT(INOUT) :: S_MMS

        ! define local variables

        COMPLEX(KIND=rDef), DIMENSION(np4,np4) :: aa_before,&
            bb_before
        LOGICAL :: badcol, &
            badrow

        LOGICAL, DIMENSION(np4) :: col, &
            row

        COMPLEX(KIND=rDef) :: c0, &
            ci

        COMPLEX(KIND=rDef), DIMENSION(np4) :: cvct

        INTEGER :: i, &
            j, &
            k, &
            info, &
            nmax4, h 

        REAL(KIND=rDef) :: as, &
            eps, &
            gamco, &
            r, &
            rm, &
            rs

        ci      = CMPLX(0.0_rDef,1.0_rDef,rDef)
        eps     = 1.e-4_rDef
        !
        ! Compute convected wavenumbers.  Store them in a file.
        do j=1,np
            rm = rmx(j)
            rs = rmt(j)
            as = snd(j)
            r  = rr(j)
            !       print*,'ak = ',ak,' as = ',as,' rm = ',rm
            !      JS: could this be from the following train of thought?
            !
            !      Looking along the diagonal:
            !      [A] {x} = \lambda [B} {x} 
            !      [A]     = \lambda [B}  
            !      where \lambda = -i gam
            !      
            !      taking a diagonal term, (any one)
            !      -1i ( k/A - m/r M_th) = -1i gam_cvct M_x
            !      gam_cvct = 1/M_x ( -1i ( k/A - m/r M_th) )

            if ( (rm.ne.0.0_rDef) .and. (r.ne.0.0_rDef) ) then
                cvct(j) = (ak/as -REAL(mm,rDef)*rs/r)/rm
            endif
        enddo
        open(unit=22,               &
            file='cv.waves.dat',   &
            status='unknown')
        rewind 22
        do j=1,np
            write(22,19) cvct(j)
        enddo
        ! write(6,17) (cvct(j), j=1,np)
        17   format(1x,'Convected wavenumbers: ',/,8(f10.5))
        19   format(1x,2e15.5)
        close(22)
        !
        badcol = .false.
        do j=1,np4
            col(j) = .true.
            do k=1,np4
                !                WRITE(6,*) aa(k,j)
                if ((abs(aa(k,j)).gt.eps) ) then !.or.(isnan(REAL(aa(k,j),rDef)))) then
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
                !  write(6,25) k
                badrow = .true.
            endif
        enddo
        !
        if (badrow.or.badcol) return
        20   format(1x,'Column ',i4,' contains all zeros.')
        25   format(1x,'Row    ',i4,' contains all zeros.')
        !
        ! print *, jobvl, jobvr
        !
        !     CALL ZGEGV(JOBVL,JOBVR,np4,aa,NMAX4,bb,NMAX4,ALPHA,BETA, &
        !                VL,NMAX4,VR,NMAX4,WORK,2*NMAX4,RWORK,INFO )

        nmax4 = np4

        !     CALL ZGEGV(JOBVL,JOBVR,np4,aa,NMAX4,bb,NMAX4,ALPHA,BETA, &
        !                VL,NMAX4,VR,NMAX4,WORK,2*NMAX4,RWORK,INFO )
        !
        ! updated call
        !
        aa_before = aa
        bb_before = bb
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

        !!       DO i = 1,np4
        !       IF (REAL(BETA(i)).gt.eps) THEN
        !        S_MMS(:,i) =  MATMUL(aa_before,VR(:,i)) - (ALPHA(i)/BETA(i))*MATMUL(bb_before,VR(:,i))
        !        ELSE
        !           WRITE(6,*) i 
        !       ENDIF
        !       END DO
        ! write(6,960) info
        960  format(1x,'info = ',i3)
        !

        c0  = CMPLX(0.0_rDef,0.0_rDef,rDef)
        !
        ! Compute cut-off wavenumber for uniform flow. Eqn(4.3)
        !      if ((ir.eq.1) .and. (slp.eq.0.0_rDef) .and. (is.eq.0)) then
        !       rm = rmx(1)
        !       gamco = REAL(ak,rDef)*rm/(rm*rm -1.0_rDef)
        !      write(6,30) gamco
        !      endif
        ! 30   format(/,1x,'Cut-off wavenumber: ',e15.5,/)
        !
        ! Print the gammas to the display.
        !      write(6,500)
        500  format(1x)
        !      write(6,50)


        ! 
        ! JS: Calculate the eigenvalues/axial wavenumbers, "gam", while sorting out the
        ! trivial results based on an error tolerance, "eps". For the gammas that
        ! remain, caluclate the  axial group velocity, (from Equation 4.4)
        !
        ! We do this because the eigenvalues are returned in the form,
        ! alpha/beta, and since we can't have a 0 denominator we have to 
        ! take out the corresponding eigenvalue

        do j=1,np4
            ! JS: if the beta is not zero then we can proceed 
            if (beta(j).ne.c0) then
                gam(j) = ci*alpha(j)/beta(j)
                if (abs(AIMAG(gam(j))).lt.eps) then
                    gam(j) = CMPLX(REAL(gam(j)),0.0d0,rDef)
                endif
                vphi(j)  = ak/gam(j)
                ! write(6,10) j,gam(j),gam(j)/ak,vphi(j)
            endif
            if (beta(j).eq.c0) then
                ! JS: We found a zero!
                ! WRITE(6,*) j, 'has a zero beta:', beta(j)
            endif
        enddo
        970  format(1x,i4,4e13.4)



        ! WRITE(6,*) 'Print all the gammas to a file.'
        !      open(unit=15,file='gammas.dat',status='unknown')
        !    open(unit=35,file='gam.acc',status='unknown')
        !    open(unit=55,file='gammasOnly.dat',status='unknown')
        !;      rewind 15
        !    rewind 35
        !     rewind 55
        !     write(15,50)
        !      write(35,55)
        ! 50   format('#',3x,'j',7x,'Re{gam}',7x,'Im{gam}',6x,'Re{gam}/k', &
        !         6x,'Im{gam}/k',6x,'kappa')
        ! 55   format('#',3x,'j',10x,'Re{gam}',13x,'Im{gam}',11x,'Re{gam/ak}', &
        !         10x,'Im{gam/ak}',5x,'nz')




        ! In the case of linear shear with no sloppe and no swirl, the following is 
        ! done instead

        !      do i = 1,np4
        !       if ((ir.eq.1) .and. (slp.eq.0.0_rDef) .and. (is.eq.0)) then
        !        rm   = rmx(1)
        !!       akap(i) = (rm*rm -1.)*gam(i)*gam(i) &
        !!          -2.*real(ak)*rm*gam(i) +real(ak)*real(ak)
        !        akap(i) = (rm*rm -1.0_rDef)*REAL(gam(i)*gam(i),rDef) &
        !           -2.0_rDef*REAL(ak,rDef)*rm*REAL(gam(i),rDef)      &
        !                  +REAL(ak,rDef)*REAL(ak,rDef)
        !        if (akap(i).gt.0.0_rDef) then
        !         akap(i) = SQRT(akap(i))
        !        write(15,10) i,gam(i),gam(i)/ak,vphi(i),akap(i)
        !        else
        !       write(15,10) i,gam(i),gam(i)/ak,vphi(i)
        !        endif
        !       endif
        !       write(35,12) i,gam(i),gam(i)/ak,vphi(i)
        !       write(15,10) i,gam(i),gam(i)/ak,vphi(i)
        !       write(55,*) REAL(gam(i)),AIMAG(gam(i))
        !      enddo
        !     close(15)
        !    close(35)
        !    close(55)
        10   format(1x,i4,9e15.6)
        12   format(1x,i4,6e20.12)
        !
        return

        end

    END MODULE analysisModule
