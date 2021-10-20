MODULE rmachModule
      USE, INTRINSIC :: ISO_FORTRAN_ENV
      ! USE Akima1D
      IMPLICIT NONE
      PRIVATE
      PUBLIC :: rmach

INTERFACE rmach
      MODULE PROCEDURE rmach1
END INTERFACE

      INTEGER, PARAMETER :: rDef = REAL64

CONTAINS

      SUBROUTINE rmach1(npts,rmch,drm,dd)

      INTEGER, INTENT(IN) :: &
          npts
      

      REAL(KIND=rDef), DIMENSION(:), INTENT(OUT) :: &
          drm, &
          rmch

      REAL(KIND=rDef), DIMENSION(:,:), INTENT(IN) :: &
          dd


! local variables


      INTEGER :: &
          j, &
          k

      REAL(KIND=rDef) :: &
          tot
      

!
! drh mod:  read in data and use akima spline
!
       ! open(unit=22,file='mach.input',status='unknown')
       ! READ(22,*) nptsIn

       ! ALLOCATE(rIn(nptsIn), &
       !       rmchIn(nptsIn))

       ! DO i=1,nptsIn
       !  READ(22,*) rIn(i),rmchIn(i)
       ! END DO

       ! CLOSE(22)
!
! spline data onto grid
!
! JS: removed read from file capability, now it is input from main

       ! CALL Akima433Interpolation(inputDataLength  = nptsIn, &
       !                            xInputData       = rIn,    &
       !                            yInputData       = rmchIn, &
       !                            outputDataLength = npts,   &
       !                            xOutputData      = rr,     &
       !                            yOutputData      = rmch)

       ! DEALLOCATE(rIn, rmchIn)

!      read (22,*) (rmch(i), i = 1,npts)
!      close(22)
!
! Spectral computation of M'.
       do k=1,npts
        tot = 0.0_rDef
        do j=1,npts
         tot = tot +dd(k,j)*rmch(j)
        enddo
        drm(k) = tot
       enddo

      END SUBROUTINE rmach1
END MODULE rmachModule
