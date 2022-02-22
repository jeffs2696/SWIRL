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

MODULE MPI_TYPE_COMMITCall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_TYPE_COMMIT call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPITypeCommit

CONTAINS

SUBROUTINE MPITypeCommit(DATATYPE,IERROR)
  INTEGER, INTENT(INOUT)  :: DATATYPE
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_TYPE_COMMIT(DATATYPE,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE MPITypeCommit

END MODULE MPI_TYPE_COMMITCall

MODULE MPI_TYPE_COMMITModule
  USE MPI_TYPE_COMMITCall

! this module is the wrapper to make the new F95 MPI_TYPE_COMMIT
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_TYPE_COMMIT

CONTAINS

SUBROUTINE HTMPI_TYPE_COMMIT(DATATYPE,IERROR)
  INTEGER, INTENT(INOUT) :: DATATYPE
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPITypeCommit(DATATYPE = DATATYPE, &
                     IERROR   = IERROR)

  RETURN
END SUBROUTINE HTMPI_TYPE_COMMIT

END MODULE MPI_TYPE_COMMITModule
