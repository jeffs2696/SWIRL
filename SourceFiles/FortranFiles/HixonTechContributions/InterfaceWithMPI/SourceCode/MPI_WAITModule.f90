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

MODULE MPI_WAITCall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_WAIT call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: WaitForMPIMessage

CONTAINS

SUBROUTINE WaitForMPIMessage(REQUEST, STATUS, IERROR)
  INTEGER, INTENT(INOUT) :: REQUEST ! handle
  INTEGER, DIMENSION(MPI_STATUS_SIZE), INTENT(OUT) :: STATUS
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_WAIT(REQUEST,STATUS,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE WaitForMPIMessage

END MODULE MPI_WAITCall

MODULE MPI_WAITModule
  USE MPI_WAITCall
  USE IncludeMPIImplementation, ONLY: MPI_STATUS_SIZE

! this module is the wrapper to make the new F95 MPI_WAIT
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_WAIT

CONTAINS

SUBROUTINE HTMPI_WAIT(REQUEST, STATUS, IERROR)
  INTEGER, INTENT(INOUT) :: REQUEST ! handle
  INTEGER, DIMENSION(MPI_STATUS_SIZE), INTENT(OUT) :: STATUS
  INTEGER, INTENT(OUT) :: IERROR

  CALL WaitForMPIMessage(REQUEST = REQUEST, &
                         STATUS  = STATUS,  &
                         IERROR  = IERROR)

  RETURN
END SUBROUTINE HTMPI_WAIT

END MODULE MPI_WAITModule
