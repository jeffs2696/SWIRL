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

        INTEGER, INTENT(IN) :: np

        REAL(KIND=rDef), DIMENSION(:),INTENT(IN) :: rr
        COMPLEX(KIND=rDef), DIMENSION(:,:), INTENT(IN) :: vrm

! Initialized character to append at the end of the file name
        CHARACTER(3) ::ff
        CHARACTER(50):: basen,basem
        INTEGER :: j,n, m, np4, nn, myunit1!, myunit2
        !INTEGER :: i,j,k,n, mf, m, np4,mn, mx, nf, delta_egv,ncols
        !INTEGER, DIMENSION(1000) :: nfile,mfile
        np4 = np*4
        ! the output is currently done such that the real values are stored in basen and imaginary values are stored with basem
        DO n = 1,np4
            m = n + 10000
            nn = n + 500
            ! assign a value to the character variable ff, to be appended to the file names
            WRITE(ff,'(i3.3)') n

! redefining file name for each eigenvalue
            basen    = '04-EVanalysis/egvre.'
            basem    = '04-EVanalysis/egvim.'

            ! open(unit=nn,file=basen//ff)!,status='old')
            ! open(unit=m,file=basem//ff)!,status='old') 


            open(newunit=myunit1,file=TRIM(ADJUSTL(basen))//ff)!,status='old')
            !open(newunit=myunit2,file=basem//ff,status='old')


!5    format('#',2i4,100e10.2)
!        WRITE(n) 'Rad             v_r          v_t        v_x        p'
!         WRITE(n,'(4a15)') [CHARACTER(26) :: 'Rad','v_r','v_t','v_x','p   ']
            DO j = 1,np
                !write(nn,500) rr(j),real(vrm(j,n)),real(vrm(j+1*np,n)),real(vrm(j+2*np,n)),real(vrm(j+3*np,n))
                !write(m,500) rr(j),aimag(vrm(j,n)),aimag(vrm(j+1*np,n)),aimag(vrm(j+2*np,n)),aimag(vrm(j+3*np,n))
            ENDDO

            close(myunit1)
            !close(myunit2)
            
            
        ENDDO
        !      endif
!500     format(16e12.3)
        do n = 1,np4
        enddo

    END SUBROUTINE saveEGV2

END MODULE egvModule
