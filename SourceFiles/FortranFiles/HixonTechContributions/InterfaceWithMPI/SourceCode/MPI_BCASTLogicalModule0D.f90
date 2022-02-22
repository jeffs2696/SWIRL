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

MODULE MPI_BCASTLogicalCall0D

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_BCAST call for logical data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIBCastLogical0D

CONTAINS

SUBROUTINE MPIBCastLogical0D(BUF,COUNT,DATATYPE,ROOT,COMM,IERROR)
  LOGICAL, INTENT(INOUT) :: BUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: ROOT
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_BCAST(BUF,COUNT,DATATYPE,ROOT,COMM,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE MPIBCastLogical0D

END MODULE MPI_BCASTLogicalCall0D

MODULE MPI_BCASTLogicalModule0D
  USE MPI_BCASTLogicalCall0D

! this module is the wrapper to make the new F95 MPI_BCAST
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_BCAST

INTERFACE HTMPI_BCAST
  MODULE PROCEDURE MPI_BCASTLogical0D
END INTERFACE HTMPI_BCAST

CONTAINS

SUBROUTINE MPI_BCASTLogical0D(BUF,COUNT,DATATYPE,ROOT,COMM,IERROR)
  LOGICAL, INTENT(INOUT) :: BUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: ROOT
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPIBCastLogical0D(BUF      = BUF,      &
                         COUNT    = COUNT,    &
                         DATATYPE = DATATYPE, &
                         ROOT     = ROOT,     &
                         COMM     = COMM,     &
                         IERROR   = IERROR)

  RETURN
END SUBROUTINE MPI_BCASTLogical0D

END MODULE MPI_BCASTLogicalModule0D
