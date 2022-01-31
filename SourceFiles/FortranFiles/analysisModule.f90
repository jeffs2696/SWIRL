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

    subroutine analysis1(&
        np,&
        np4, &
        ak, &
        rr, &
        snd, &
        rmx, &
        rmt, &
        aa, &
        bb, &
        alpha, &
        beta, &
        vl, &
        vr, &
        work, &
        rwork, &
        gam, &
        jobvl, &
        jobvr, &
        mm, &
        ir, &
        is, &
        vphi, &
        akap)

        INTEGER, INTENT(IN) :: &
            np, &
            np4, &
            mm, &
            ir, &
            is

        REAL(KIND=rDef), DIMENSION(:), INTENT(IN) :: &
            rr, &
            snd, &
            rmx, &
            rmt, &
            rwork

        REAL(KIND=rDef), DIMENSION(:), INTENT(OUT) :: &
            akap

        COMPLEX(KIND=rDef), INTENT(IN) :: &
            ak ! axial wavenumber      

        COMPLEX(KIND=rDef), DIMENSION(:), INTENT(IN) :: &
            alpha, &
            beta, &
            work

        COMPLEX(KIND=rDef), DIMENSION(:), INTENT(OUT) :: &
            gam, &
            vphi

        COMPLEX(KIND=rDef), DIMENSION(:,:), INTENT(IN) :: &
            aa, &
            bb, &
            VL, &
            VR

        CHARACTER, INTENT(IN) :: &
            jobvl, &
            jobvr

! define local variables

        LOGICAL :: &
            badcol, &
            badrow

        LOGICAL, DIMENSION(np4) :: &
            col, &
            row 

        COMPLEX(KIND=rDef) :: &
            c0, &
            ci, &
            SMALL

        COMPLEX(KIND=rDef), DIMENSION(np4) :: &
            cvct

        COMPLEX(KIND=rDef), DIMENSION(np4,np4) :: aa_before, bb_before 

        INTEGER :: &
            i, &
            j, &
            k, &
            info, &
            nmax4

        REAL(KIND=rDef) :: &
            as, &
            eps, &
        ! gamco, &
            r, &
            rm, &
            rs
        LOGICAL :: debug = .FALSE. 

        INTEGER  :: &
            UNIT 

        CHARACTER(10) :: &
            file_id
        CHARACTER :: &
            file_name

        ci      = CMPLX(0.0_rDef,1.0_rDef,rDef)

        eps     = 1.e-12_rDef !JS: is this sufficient

! Compute convected wavenumbers.  Store them in a file.
        do j=1,np

            ! get mean flow, i.e. . .
            rm = rmx(j) !axial ,. . . 
            rs = rmt(j) !and tangential mach numbers +  ...
            as = snd(j) ! the speed of sound.
            r  = rr(j)  ! Don't forget, we need this data at each radial point! 

            IF (debug) THEN 
                ! WRITE(6,*) 'ak = ',ak,' Mt = ',as,' Mx = ',rm
            ELSE
            ENDIF

            IF ( (rm.ne.0.0_rDef) .and. (r.ne.0.0_rDef) ) THEN

                cvct(j) = (ak/CMPLX(as,KIND=rDef) - CMPLX(mm,KIND=rDef)*CMPLX(rs,KIND=rDef)/CMPLX(r,KIND=rDef))/CMPLX(rm,KIND=rDef)

            ENDIF

        ENDDO

        file_name = 'cv.waves.dat'

        OPEN(NEWUNIT=UNIT,FILE=file_name)
        DO j=1,np
            WRITE(UNIT,19) cvct(j)
            IF (debug) THEN
                ! WRITE(0,19) cvct(j)
            ELSE
            ENDIF
        ENDDO

        IF (debug) THEN
            WRITE(0,17) (cvct(j), j=1,np)

        ELSE
        ENDIF
17      FORMAT(1x,'Convected wavenumbers: ',/,8(f10.5))
19      FORMAT(1x,2e15.5)

        CLOSE(UNIT)

! Check for zero rows and columns in A.

        badcol = .FALSE.

        DO j=1,np4

            col(j) = .TRUE.

            DO k=1,np4

                IF (abs(aa(k,j)).gt.eps) THEN

                    col(j) = .false.

                ENDIF

            ENDDO

        ENDDO

        DO j=1,np4

            IF (col(j)) THEN


                    WRITE(6,20) j

                badcol = .true.

            ENDIF

        ENDDO

        badrow = .false.

        DO k=1,np4

            row(k) = .true.

            DO j=1,np4

                IF (abs(aa(k,j)).gt.eps) then

                    row(j) = .false.

                ENDIF

            ENDDO

        ENDDO

        do k=1,np4

            if (row(k)) then

                    write(6,25) k
                    badrow = .true.
            endif

        enddo
!
        if (badrow.or.badcol) return
 20      format(1x,'Column ',i4,' contains all zeros.')
 25      format(1x,'Row    ',i4,' contains all zeros.')

        nmax4 = np4

        aa_before = aa
        bb_before = bb

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

        !WRITE(6,*) 'INFO = ' ,INFO
        IF (INFO .EQ. 0) THEN
            WRITE(0,*) 'EIGENSOLVER PASSED'
        ELSEIF (INFO .EQ. 1 .or. INFO .LT. np4) THEN
            WRITE(0,*) 'EIGENSOLVER FAILED'
            WRITE(0,*) 'The QZ iteration. No eigenvectors are calculated'
            WRITE(0,*) 'But ALPHA(j) and BETA(j) should be correct for  '
            WRITE(0,*) 'j = INFO + 1,...,N'
        ELSEIF (INFO .LT. 0) THEN 
            WRITE(0,*) 'EIGENSOLVER FAILED'
            WRITE(6,*) 'if INFO = -i, the i-th argument had an illegal value.'
        ENDIF



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

        SMALL = CMPLX(eps,eps,KIND=rDef)

        WRITE(file_id, '(i0)') np 
        OPEN(NEWUNIT=UNIT,FILE='alpha_beta'//TRIM(ADJUSTL(file_id)) // '.dat')

        DO j=1,np4

            IF ( (ABS(REAL(alpha(j))).LT.ABS(eps)) .or. &
                (ABS(AIMAG(alpha(j))).LT.ABS(eps)) ) THEN
!                WRITE(6,*) 'Eigenvalue (',j,')',' is numerically infinite or undetermined'
!                WRITE(6,*) 'ALPHA(',j,') = ', alpha(j)
!                WRITE(6,*) 'BETA (',j,') = ', beta(j)
!
                ELSE IF ( (ABS(REAL(beta(j))).LT.ABS(eps)) .or. &
                (ABS(AIMAG(beta(j))).LT.ABS(eps)) ) THEN
                !WRITE(6,*) 'Eigenvalue (',j,')',' is numerically infinite or undetermined'
                !WRITE(6,*) 'ALPHA(',j,') = ', alpha(j)
                !WRITE(6,*) 'BETA (',j,') = ', beta(j)
            ELSE 
                WRITE(UNIT,*) j,alpha(j),beta(j)
                if (beta(j).ne.c0) then

                    gam(j) = ci*alpha(j)/beta(j)


                    if (abs(AIMAG(gam(j))).lt.eps) then

                        gam(j) = CMPLX(REAL(gam(j)),0.0d0,rDef)

                    else
                        ! WRITE(6,*) 'Bad Eigenvalue at' , j

                    endif
                    vphi(j)  = ak/gam(j)

                    IF (debug) THEN 
                        ! write(6,10) j,gam(j),gam(j)/ak,vphi(j)
                    ELSE
                    ENDIF
                ELSE

                    WRITE(6,*) 'Bad Eigenvalue at' , j
                endif
            ENDIF

        enddo
        CLOSE(UNIT)
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

        do i = 1,np4
! JS: if there is (linear shear) and (no slope) and (no swirl then) ...
! Note: this will never happen because we removed the swrl.input functionality 
            if ((ir.eq.1) .and.  (is.eq.0)) then
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

        ! SS = MATMUL(aa_before,VR(:,1)) - gam(1)*MATMUL(bb_before,VR(:,1))
        ! return

    end

END MODULE analysisModule
