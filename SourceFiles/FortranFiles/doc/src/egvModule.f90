MODULE egvModule
    USE, INTRINSIC :: ISO_FORTRAN_ENV
    IMPLICIT NONE
    PRIVATE
    PUBLIC :: saveEGV
    INTERFACE saveEGV
        !MODULE PROCEDURE saveEGV1
        MODULE PROCEDURE saveEGV2
    END INTERFACE

    INTEGER, PARAMETER :: rDef = REAL64

CONTAINS

!    SUBROUTINE saveEGV1(np,np4,vrm,rr)
!        ! this was initially commented out in the original code
!        INTEGER, INTENT(IN) :: np,&
!            np4
!
!        REAL(KIND=rDef), DIMENSION(:),INTENT(IN) :: rr
!        COMPLEX(KIND=rDef), DIMENSION(:,:), INTENT(IN) :: vrm
!
!! Initialized character to append at the end of the file name
!        CHARACTER(2) ::ff
!        CHARACTER(15)::basen,basem
!        INTEGER :: j,n, mf,  delta_egv, mn, mx, k, ncols, nf
!!        INTEGER :: i,j,k,n, mf, mn, mx, nf, delta_egv,ncols
!        INTEGER, DIMENSION(1000) :: nfile,mfile
!
!! the output is currently done such that the real values are stored in basen and imaginary values are stored with basem
!        delta_egv = 2
!        ncols = np4/delta_egv
!        do n = 1,ncols+1
!            ! assign a value to the character variable ff, to be appended to the file names
!            write(ff,'(i2.2)') n
!            nfile(n) = 25 +n
!            mfile(n) = 5500 +n
!
!!            WRITE(0,*) nfile(n), mfile(n)
!! redefining file name for each eigenvalue
!            basen    = '04-EVanalysis/egvre.'
!            basem    = '04-EVanalysis/egvim.'
!            open(newunit=nfile(n),file=basen//ff,status='unknown')
!            open(unit=mfile(n),file=basem//ff,status='unknown')
!!            rewind nfile(n)
!!            rewind mfile(n)
!        enddo
!!5    format('#',2i4,100e10.2)
!!      if (egv .eq. 'V') then
!        do j=1,np
!            do n = 1,ncols+1
!                nf  = nfile(n)
!                mf  = mfile(n)
!                mn  = (n -1)*delta_egv +1
!                mx  = n*delta_egv
!                if (n.eq.ncols+1) then
!                    mx = min(mx,np4)
!                endif
!                write(nf,500) rr(j),(real(vrm(j+3*np,k)), k=mn,mx)
!                write(mf,500) rr(j),(aimag(vrm(j+3*np,k)), k=mn,mx)
!            enddo
!        enddo
!        !      endif
!500     format(16e12.3)
!
!        do n = 1,ncols+1
!            close(nfile(n))
!            close(mfile(n))
!        enddo
!
!    END SUBROUTINE saveEGV1
    SUBROUTINE saveEGV2(np,vrm,rr)

        INTEGER, INTENT(IN) :: &
            np
        REAL(KIND=rDef), DIMENSION(:),INTENT(IN) :: &
            rr
        COMPLEX(KIND=rDef), DIMENSION(:,:), INTENT(IN) :: &
            vrm
!
! Initialized character to append at the end of the file name
        CHARACTER(10) :: &
            ff

        CHARACTER(1000) :: &
            formatHeader, &
            formatData, &
            basen     


        COMPLEX(KIND=rDef) ::&
            ci


        REAL(KIND=rDef) :: phi

        INTEGER :: j,n,  np4,  myunit1

        ci  = CMPLX(0.0_rDef,1.0_rDef,rDef)


        formatHeader = "(13a16)"
        formatData   = "(13f16.12)"
        np4 = np*4
        ! the output is currently done such that the real values are stored in basen and imaginary values are stored with basem
        DO n = 1,np4
            ! assign a value to the character variable ff, to be appended to the file names
            WRITE(ff,'(i0.4)') n

! redefining file name for each eigenvalue
            basen    = '04-EVanalysis/egv.'

            open(newunit=myunit1,file=TRIM(ADJUSTL(basen))//TRIM(ADJUSTL(ff)))!,status='old')

            WRITE(myunit1,formatHeader) &
            'Rad','v_r[Re]','v_t[Re]','v_x[Re]','p[Re]  ','p_mg[Re]','p_no_phase[Re]', &
                  'v_r[Im]','v_t[Im]','v_x[Im]','p[Im]  ','p_mg[Im]','p_no_phase[Im]'

            phi = atan2(aimag(vrm(np4,n)),real(vrm(np4,n)))
            DO j = 1,np
                write(myunit1,formatData) &
                    rr(j),&
                    real(vrm(j,n)), &
                    real(vrm(j+1*np,n)), &
                    real(vrm(j+2*np,n)), &
                    real(vrm(j+3*np,n)), &
                    SQRT(real(vrm(j+3,n))**2.0_rDef+aimag(vrm(j+3,n))**2.0_rDef) , &
                    real(vrm(j+3*np,n)*exp(-ci*CMPLX(phi,KIND=rDef))), &
                    aimag(vrm(j,n)), &
                    aimag(vrm(j+1*np,n)), &
                    aimag(vrm(j+2*np,n)), &
                    aimag(vrm(j+3*np,n)) ,&
                    SQRT(real(vrm(j+3,n))**2.0_rDef+aimag(vrm(j+3,n))**2.0_rDef),&
                    aimag(vrm(j+3*np,n)*exp(-ci*CMPLX(phi,KIND=rDef)))
            ENDDO

            close(myunit1)
            
            
        ENDDO

    END SUBROUTINE saveEGV2

END MODULE egvModule
