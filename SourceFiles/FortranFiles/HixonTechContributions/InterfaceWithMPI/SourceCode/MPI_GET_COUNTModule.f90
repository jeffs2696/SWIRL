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

MODULE MPI_GET_COUNTCall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_GET_COUNT call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: GetMPIMessageCount

CONTAINS

SUBROUTINE GetMPIMessageCount(STATUS, DATATYPE, COUNT, IERROR)
  INTEGER, DIMENSION(MPI_STATUS_SIZE), INTENT(IN) :: STATUS
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(OUT) :: COUNT
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_GET_COUNT(STATUS,DATATYPE,COUNT,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE GetMPIMessageCount

END MODULE MPI_GET_COUNTCall

MODULE MPI_GET_COUNTModule
  USE MPI_GET_COUNTCall
  USE IncludeMPIImplementation, ONLY: MPI_STATUS_SIZE

! this module is the wrapper to make the new F95 MPI_GET_COUNT
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_GET_COUNT

CONTAINS

SUBROUTINE HTMPI_GET_COUNT(STATUS, DATATYPE, COUNT, IERROR)
  INTEGER, DIMENSION(MPI_STATUS_SIZE), INTENT(IN) :: STATUS
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(OUT) :: COUNT
  INTEGER, INTENT(OUT) :: IERROR

  CALL GetMPIMessageCount(STATUS   = STATUS,   &
                          DATATYPE = DATATYPE, &
                          COUNT    = COUNT,    &
                          IERROR   = IERROR)

  RETURN
END SUBROUTINE HTMPI_GET_COUNT

END MODULE MPI_GET_COUNTModule
