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
            ci

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
        LOGICAL :: debug = .TRUE.
!
!        INTEGER  :: &
!            UNIT , &
!            UNIT2 , &
!            UNIT3
!
        CHARACTER(10) :: &
            file_id
        CHARACTER(26) :: &
            file_name

        ci      = CMPLX(0.0_rDef,1.0_rDef,rDef)

        eps     = 10.e-4_rDef !JS: is this sufficient

! Compute convected wavenumbers.  Store them in a file.
        do j=1,np

            ! get mean flow, i.e. . .
            rm = rmx(j) !axial ,. . .
            rs = rmt(j) !and tangential mach numbers +  ...
            as = snd(j) ! the speed of sound.
            r  = rr(j)  ! Don't forget, we need this data at each radial point!
! Check Convective wave number calculation
! what is 'as' is zero??? - JS
            IF ( (rm.ne.0.0_rDef) .and.(r.gt.0.0_rDef) ) THEN

                cvct(j) = (&
                    ak/CMPLX(as,KIND=rDef) &
                    - CMPLX(mm,KIND=rDef)*CMPLX(rs,KIND=rDef)/CMPLX(r,KIND=rDef)&
                    )&
                    /CMPLX(rm,KIND=rDef)

            ENDIF

        ENDDO

        file_name = '04-EVanalysis/cv.waves.dat'


        ! OPEN(NEWUNIT=UNIT,FILE=file_name)
        ! WRITE(UNIT,*) 'REAL ' , 'IMAG'

        DO j=1,np

            ! WRITE(UNIT,*) REAL(cvct(j),KIND=rDef), AIMAG(cvct(j))

            IF (debug) THEN
!                WRITE(0,19) cvct(j)
            ELSE
            ENDIF

        ENDDO

        IF (debug) THEN
            !WRITE(0,17) (cvct(j), j=1,np)

        ELSE
        ENDIF
!17      FORMAT(1x,'Convected wavenumbers: ',/,8(f10.5))
!19      FORMAT(1x,2e15.5)

        ! CLOSE(UNIT)

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


                WRITE(0,*) j
                !WRITE(0,20) j

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

                ! WRITE(0,25) k
                WRITE(0,*) k
                badrow = .true.
            endif

        enddo
!
        if (badrow.or.badcol) return
!20      format(1x,'Column ',i4,' contains all zeros.')
!25      format(1x,'Row    ',i4,' contains all zeros.')

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
            LWORK = 2*NMAX4, & !2*NMAX4, & ! LWORK
            RWORK = RWORK,   & ! RWORK
            INFO  = INFO )     ! INFO

        IF ((INFO .EQ. 0).and.(debug.eqv..TRUE.)) THEN
            WRITE(0,*) 'WORK = ' ,WORK(1)
            WRITE(0,*) 'LWORK = ' ,2*NMAX4
            WRITE(0,*) 'INFO = ' ,INFO
            WRITE(0,*) 'EIGENSOLVER PASSED'
        ELSEIF ((INFO .EQ. 1 .or. INFO .LT. np4).and.(debug.eqv..TRUE.)) THEN
            WRITE(0,*) 'EIGENSOLVER FAILED'
            WRITE(0,*) 'The QZ iteration. No eigenvectors are calculated'
            WRITE(0,*) 'But ALPHA(j) and BETA(j) should be correct for  '
            WRITE(0,*) 'j = INFO + 1,...,N'
        ELSEIF ((INFO .LT. 0).and.(debug.eqv..TRUE.)) THEN
            WRITE(0,*) 'INFO = ' ,INFO
            WRITE(0,*) 'EIGENSOLVER FAILED'
            WRITE(0,*) 'if INFO = -i, the i-th argument had an illegal value.'
        ENDIF

        c0  = CMPLX(0.0_rDef,0.0_rDef,rDef)
!
! Compute cut-off wavenumber for uniform flow.
! getting -Werror=compare-reals error with the if statements

! if there is only axial flow, no shear or swirl, then... JS

        !  if ((ir.eq.1) .and. (slp.eq.0.0_rDef) .and. (is.eq.0)) then
        !      rm = rmx(1)
        !      gamco = REAL(ak,rDef)*rm/(rm*rm -1.0_rDef)
        !      WRITE(0,30) gamco
        !  endif
! 30      format(/,1x,'Cut-off wavenumber: ',e15.5,/)

!
! Print the gammas to the display.
!        WRITE(0,500)
!500     format(1x)
!
!        WRITE(0,50)


        WRITE(file_id, '(i0.4)') np

        !WRITE(0,50)
        ! OPEN(NEWUNIT=UNIT,FILE='04-EVanalysis/' //'gammas'//TRIM(ADJUSTL(file_id)) // '.dat')
        ! OPEN(NEWUNIT=UNIT2,FILE='04-EVanalysis/' //'gam'//TRIM(ADJUSTL(file_id)) // '.acc')
        ! OPEN(NEWUNIT=UNIT3,FILE='04-EVanalysis/' //'gammasOnly'//TRIM(ADJUSTL(file_id)) // '.dat')
        ! WRITE(UNIT2,50)
        ! WRITE(UNIT,55)

        do j=1,np4
            ! WRITE(0,*) 'before' ,j, alpha(j),beta(j)
            if ((beta(j).ne.c0) .or. (REAL(beta(j)).gt.eps) .or.(AIMAG(beta(j)).gt.eps)) then
                gam(j) = ci*alpha(j)/beta(j)
                if (abs(AIMAG(gam(j))).lt.eps) then
                    gam(j) = CMPLX(REAL(gam(j)),0.0d0,rDef)
                elseif (abs(REAL(gam(j))).lt.eps) then
                    WRITE(0,*)
                    gam(j) =CMPLX(0.0_rDef,AIMAG(gam(j)),KIND=rDef)
                endif
                vphi(j)  = ak/gam(j)
                ! WRITE(0,10) j,gam(j),gam(j)/ak,vphi(j)
                ! WRITE(UNIT ,12) j,gam(j),gam(j)/ak, vphi(j)
                ! WRITE(UNIT2,10) j,gam(j),gam(j)/ak,vphi(j)
                ! WRITE(UNIT3,*) REAL(gam(j)),AIMAG(gam(j))
            elseif (beta(j).eq.c0) THEN
                gam(j) = CMPLX(0.0_rDef,0.0_rDef, KIND=rDef) 
            elseif ((REAL(beta(j)).lt.eps) .or. AIMAG(beta(j)).lt.eps) THEN
                WRITE(0,*) 'beta is lt eps'
                ! gam(j) = ci*CMPLX(0.0_rDef, AIMAG(alpha(j)), KIND=rDef)/beta(j)
            endif

            WRITE(0,*) 'after' ,j, alpha(j),beta(j)
        enddo

        ! CLOSE(UNIT)
        ! CLOSE(UNIT2)
        ! CLOSE(UNIT3)
!
!        iO j=1,np4
!
!            IF ( ((ABS(REAL(alpha(j))).LT.ABS(eps)) .or. &
!                (ABS(AIMAG(alpha(j))).LT.ABS(eps)) ).and.&
!                (debug.eqv..TRUE.)) THEN
!                WRITE(0,*) 'Eigenvalue (',j,')',' is numerically infinite or undetermined'
!                WRITE(0,*) 'ALPHA(',j,') = ', alpha(j)
!                WRITE(0,*) 'BETA (',j,') = ', beta(j)
!                !alpha(j) = CMPLX(0.0_rDef,0.0_rDef,KIND=rDef)
!                ELSE IF ( ((ABS(REAL(beta(j))).LT.ABS(eps)) .or. &
!                (ABS(AIMAG(beta(j))).LT.ABS(eps)) ).and.&
!                    (debug.eqv..TRUE.)) THEN
!                WRITE(0,*) 'Eigenvalue (',j,')',' is numerically infinite or undetermined'
!                WRITE(0,*) 'ALPHA(',j,') = ', alpha(j)
!                WRITE(0,*) 'BETA (',j,') = ', beta(j)
!                !beta(j) = CMPLX(0.0_rDef,0.0_rDef,KIND=rDef)
!                ELSE
!                !WRITE(UNIT,*) j,alpha(j),beta(j)
!                !  When imaginary part of complex number is zero then it is real number.
!                ! thats why we can look at the real part of beta
!                IF ( (REAL(beta(j),KIND=rDef).gt.0.0_rDef) .or. &
!                    (REAL(beta(j),KIND=rDef).lt.0.0_rDef)) THEN
!                    gam(j) = ci*alpha(j)/beta(j)
!
!
!                    IF (abs(AIMAG(gam(j))).lt.eps) THEN
!
!                        gam(j) = CMPLX(REAL(gam(j)),0.0d0,rDef)
!
!                    ELSE
!
!                        IF (debug) THEN
!                            WRITE(0,*) 'Bad Eigenvalue at' , j
!                        ELSE
!                        ENDIF
!
!                    ENDIF
!                    vphi(j)  = ak/gam(j)
!
!                    WRITE(UNIT,*) j,gam(j),gam(j)/ak,vphi(j)
!                    IF (debug) THEN
!                        WRITE(0,10) j,gam(j),gam(j)/ak,vphi(j)
!                    ELSE
!                    ENDIF
!                ELSE
!
!                    IF (debug) THEN
!                        WRITE(0,*) 'Bad Eigenvalue at' , j
!                    ELSE
!                    ENDIF
!                ENDIF
!            ENDIF
!        ENDDO
        !CLOSE(UNIT)
!970  format(1x,i4,4e13.4)
!
! Print all the gammas to a file.

!        rewind UNIT
!        rewind UNIT2
!        rewind UNIT3
        ! OPEN(NEWUNIT=UNIT2,FILE='04-EVanalysis/' //'gam'//TRIM(ADJUSTL(file_id)) // '.acc')
        ! WRITE(UNIT2,50)
! 50      format('#',3x,'j',7x,'Re{gam}',7x,'Im{gam}',6x,'Re{gam}/k', &
!             6x,'Im{gam}/k',6x,'kappa')
! 55      format('#',3x,'j',10x,'Re{gam}',13x,'Im{gam}',11x,'Re{gam/ak}', &
!             10x,'Im{gam/ak}',5x,'nz')

        DO i = 1,np4
! JS: if there is (linear shear) and (no slope) and (no swirl then) ...
! Note: this will never happen because we removed the swrl.input functionality
            if ((rmx(1).gt.0.0_rDef) .and.  (rmt(1).eq.0.0_rDef)) then
                !if ((ir.eq.1) .and.  (is.eq.0)) then
                rm   = rmx(1)
!       akap(i) = (rm*rm -1.)*gam(i)*gam(i) &
!          -2.*real(ak)*rm*gam(i) +real(ak)*real(ak)
                akap(i) = (rm*rm -1.0_rDef)*REAL(gam(i)*gam(i),rDef) &
                    -2.0_rDef*REAL(ak,rDef)*rm*REAL(gam(i),rDef)      &
                    +REAL(ak,rDef)*REAL(ak,rDef)

                IF (akap(i).gt.0.0_rDef) then
                    akap(i) = SQRT(akap(i))
                    ! WRITE(UNIT2,10) i,gam(i),gam(i)/ak,vphi(i),akap(i)
                ELSE
                    ! WRITE(UNIT2,10) i,gam(i),gam(i)/ak,vphi(i)
                ENDIF

! JS: if there is not linear shear and there is no swirl flag then proceed

            ENDIF

! because gam has blank entries (gives 1e-310 because previous loop omitted some entries)



            !WRITE(0 ,*) i!,gam(i),gam(i)/ak!, vphi(i)
        ENDDO
! 10      format(1x,i4,9e20.12)
! 12      format(1x,i4,6e20.12)
        ! CLOSE(UNIT2)


        !return

    end

END MODULE analysisModule
