MODULE smachAndSndspdModule
    USE, INTRINSIC :: ISO_FORTRAN_ENV
    ! USE Akima1D
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: smachAndSndspd

    INTERFACE smachAndSndspd
        MODULE PROCEDURE smachAndSndspd1
    END INTERFACE smachAndSndspd

    INTEGER, PARAMETER :: rDef = REAL64

CONTAINS

    SUBROUTINE smachAndSndspd1(npts,rr,rmsw,rmswp,snd,dsn,dd)

        INTEGER, INTENT(IN) :: &
            npts

        REAL(KIND=rDef), DIMENSION(:), INTENT(IN) :: &
            rr

        REAL(KIND=rDef), DIMENSION(:,:), INTENT(IN) :: &
            dd

        REAL(KIND=rDef), DIMENSION(:), INTENT(OUT) ::&
            rmsw,  &
            rmswp, &
            dsn

        REAL(KIND=rDef), DIMENSION(:), INTENT(INOUT) ::&
            snd

! local variables

        INTEGER :: i, &
            j, &
            k

        REAL(KIND=rDef) :: &
            tot, &
            gm, &
            gm1, &
            rsw1, &
            rswi, &
            x1, &
            xi

        gm  = 1.4_rDef
        gm1 = gm -1.0_rDef

! Spectral computation of M_theta'.

        DO k=1,npts
            tot = 0.0_rDef
            DO j=1,npts
                tot = tot +dd(k,j)*rmsw(j)
            ENDDO
            rmswp(k) = tot
        ENDDO

        DO k = 1, npts
            snd(k) = 0.0_rDef
        ENDDO

! calculate the speed of sound by integration (Eq. (2.6) in paper)

! put in some fixes -- need to check this.
        DO k=1,npts

            DO i = npts-1,k,-1

                IF (rr(i).gt.0.0_rDef) then

                    rswi    = rmsw(i)*rmsw(i)/rr(i)
                    rsw1    = rmsw(i+1)*rmsw(i+1)/rr(i+1)
                    xi      = rr(i)
                    x1      = rr(i+1)
                    snd(i) = snd(i+1) +0.5_rDef*(rswi +rsw1)*(x1 -xi)

                ELSE
                    snd(i) = 2.0_rDef*rmsw(i)*(rmsw(i+1) -rmsw(i))/rr(i+1)

                ENDIF

            ENDDO

            snd(k) = exp(-0.5_rDef*gm1*snd(k))


        END DO

! get the radial derivative of the speed of sound

        do k = 1,npts
            tot = 0.0_rDef
            do j = 1,npts
                tot = tot +dd(k,j)*snd(j)
            enddo
            dsn(k) = tot
        enddo

    END SUBROUTINE smachAndSndspd1
END MODULE smachAndSndspdModule
