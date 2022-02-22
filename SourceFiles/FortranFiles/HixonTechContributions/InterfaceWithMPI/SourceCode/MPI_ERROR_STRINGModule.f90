!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!                                                                    !
!  This code copyright 2020 by Hixon Technologies, LLC.              !
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

MODULE MPI_ERROR_STRINGCall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_ERROR_STRING call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: GetMPIErrorString

CONTAINS

SUBROUTINE GetMPIErrorString(ERRORCODE, STRING, RESULTLEN, IERROR)
  INTEGER, INTENT(IN) :: ERRORCODE ! IERROR from MPI call
  CHARACTER(LEN=*), INTENT(OUT) :: STRING
  INTEGER, INTENT(OUT) :: RESULTLEN
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_ERROR_STRING(ERRORCODE,STRING,RESULTLEN,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE GetMPIErrorString

END MODULE MPI_ERROR_STRINGCall

MODULE MPI_ERROR_STRINGModule
  USE MPI_ERROR_STRINGCall

! this module is the wrapper to make the new F95 MPI_ERROR_STRING
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_ERROR_STRING

CONTAINS

SUBROUTINE HTMPI_ERROR_STRING(ERRORCODE, STRING, RESULTLEN, IERROR)
  INTEGER, INTENT(IN) :: ERRORCODE 
  CHARACTER(LEN=*), INTENT(OUT) :: STRING
  INTEGER, INTENT(OUT) :: RESULTLEN
  INTEGER, INTENT(OUT) :: IERROR

  CALL GetMPIErrorString(ERRORCODE  = ERRORCODE, &
                         STRING     = STRING,    &
                         RESULTLEN  = RESULTLEN, &
                         IERROR     = IERROR)

  RETURN
END SUBROUTINE HTMPI_ERROR_STRING

END MODULE MPI_ERROR_STRINGModule
