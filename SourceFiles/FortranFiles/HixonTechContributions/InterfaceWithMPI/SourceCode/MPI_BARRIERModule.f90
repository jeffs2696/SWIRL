!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                    !
!  This code copyright 2007 by Hixon Technologies, LLC.              !
!                                                                    !
!  Used by permission in the NASA BASS CAA code, and                 !
!   cannot be modified or used in other applications without the     !
!   express permission of Hixon Technologies, LLC.                   !
!                                                                    !
!  Contact:  Ray Hixon, Hixon Technologies, LLC.                     !
!            (440) 979-1783                                          !
!            email:  rhixon@wideopenwest.com                         !
!                                                                    !
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

MODULE MPI_BARRIERCall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_BARRIER call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: SetMPIBarrier

CONTAINS

SUBROUTINE SetMPIBarrier(COMM,IERROR)
  INTEGER, INTENT(IN)  :: COMM    ! handle
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_BARRIER(COMM,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE SetMPIBarrier

END MODULE MPI_BARRIERCall

MODULE MPI_BARRIERModule
  USE MPI_BARRIERCall

! this module is the wrapper to make the new F95 MPI_BARRIER
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_BARRIER

CONTAINS

SUBROUTINE HTMPI_BARRIER(COMM,IERROR)
  INTEGER, INTENT(IN)  :: COMM  ! handle
  INTEGER, INTENT(OUT) :: IERROR

  CALL SetMPIBarrier(COMM   = COMM,  &
                     IERROR = IERROR)

  RETURN
END SUBROUTINE HTMPI_BARRIER

END MODULE MPI_BARRIERModule
