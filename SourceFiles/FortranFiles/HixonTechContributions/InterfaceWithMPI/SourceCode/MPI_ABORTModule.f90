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

MODULE MPI_ABORTCall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_ABORT call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: AbortMPI

CONTAINS

SUBROUTINE AbortMPI(COMM,ERRORCODE,IERROR)
  INTEGER, INTENT(IN)  :: COMM    ! handle
  INTEGER, INTENT(OUT) :: ERRORCODE
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_ABORT(COMM,ERRORCODE,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE AbortMPI

END MODULE MPI_ABORTCall

MODULE MPI_ABORTModule
  USE MPI_ABORTCall

! this module is the wrapper to make the new F95 MPI_ABORT
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_ABORT

CONTAINS

SUBROUTINE HTMPI_ABORT(COMM,ERRORCODE,IERROR)
  INTEGER, INTENT(IN)  :: COMM  ! handle
  INTEGER, INTENT(OUT) :: ERRORCODE
  INTEGER, INTENT(OUT) :: IERROR

  CALL AbortMPI(COMM      = COMM,      &
                ERRORCODE = ERRORCODE, &
                IERROR    = IERROR)

  RETURN
END SUBROUTINE HTMPI_ABORT

END MODULE MPI_ABORTModule
