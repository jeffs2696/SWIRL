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

MODULE MPI_ISENDLogicalCall0D

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_ISEND call for logical data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIIsendLogical0D

CONTAINS

SUBROUTINE MPIIsendLogical0D(BUF,COUNT,DATATYPE,DEST,TAG,COMM,REQUEST,IERROR)
  LOGICAL, INTENT(IN)  :: BUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: DEST
  INTEGER, INTENT(IN)  :: TAG
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, INTENT(OUT) :: REQUEST ! handle
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_ISEND(BUF,COUNT,DATATYPE,DEST,TAG,COMM,REQUEST,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE MPIIsendLogical0D

END MODULE MPI_ISENDLogicalCall0D

MODULE MPI_ISENDLogicalModule0D
  USE MPI_ISENDLogicalCall0D

! this module is the wrapper to make the new F95 MPI_ISEND
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_ISEND

INTERFACE HTMPI_ISEND
  MODULE PROCEDURE MPI_ISENDLogical0D
END INTERFACE HTMPI_ISEND

CONTAINS

SUBROUTINE MPI_ISENDLogical0D(BUF,COUNT,DATATYPE,DEST,TAG,COMM,REQUEST,IERROR)
  LOGICAL, INTENT(IN)  :: BUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: DEST
  INTEGER, INTENT(IN)  :: TAG
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, INTENT(OUT) :: REQUEST ! handle
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPIIsendLogical0D(BUF      = BUF,      &
                         COUNT    = COUNT,    &
                         DATATYPE = DATATYPE, &
                         DEST     = DEST,     &
                         TAG      = TAG,      &
                         COMM     = COMM,     &
                         REQUEST  = REQUEST,  &
                         IERROR   = IERROR)

  RETURN
END SUBROUTINE MPI_ISENDLogical0D

END MODULE MPI_ISENDLogicalModule0D
