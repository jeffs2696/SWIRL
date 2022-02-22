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

MODULE MPI_TYPE_FREECall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_TYPE_FREE call

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPITypeFree

CONTAINS

SUBROUTINE MPITypeFree(DATATYPE,IERROR)
  INTEGER, INTENT(INOUT)  :: DATATYPE
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_TYPE_FREE(DATATYPE,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE MPITypeFree

END MODULE MPI_TYPE_FREECall

MODULE MPI_TYPE_FREEModule
  USE MPI_TYPE_FREECall

! this module is the wrapper to make the new F95 MPI_TYPE_FREE
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_TYPE_FREE

CONTAINS

SUBROUTINE HTMPI_TYPE_FREE(DATATYPE,IERROR)
  INTEGER, INTENT(INOUT) :: DATATYPE
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPITypeFree(DATATYPE = DATATYPE, &
                   IERROR   = IERROR)

  RETURN
END SUBROUTINE HTMPI_TYPE_FREE

END MODULE MPI_TYPE_FREEModule
