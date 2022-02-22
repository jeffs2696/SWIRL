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

MODULE MPI_BCASTInt4Call

  USE IncludeMPIImplementation
  USE MPIKindDefs

! this module actually makes the F77 MPI_BCAST call for real4 data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIBCastInt4

CONTAINS

SUBROUTINE MPIBCastInt4(BUF,COUNT,DATATYPE,ROOT,COMM,IERROR)
  INTEGER(KIND=int4Kind), INTENT(INOUT), DIMENSION(:) :: BUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: ROOT
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPI_BCAST(BUF(1),COUNT,DATATYPE,ROOT,COMM,IERROR)

  IF (IERROR == MPI_SUCCESS) THEN
   IERROR = 0
  END IF

  RETURN
END SUBROUTINE MPIBCastInt4

END MODULE MPI_BCASTInt4Call

MODULE MPI_BCASTInt4Module
  USE MPI_BCASTInt4Call
  USE MPIKindDefs

! this module is the wrapper to make the new F95 MPI_BCAST
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_BCAST

INTERFACE HTMPI_BCAST
  MODULE PROCEDURE MPI_BCASTInt4
END INTERFACE HTMPI_BCAST

CONTAINS

SUBROUTINE MPI_BCASTInt4(BUF,COUNT,DATATYPE,ROOT,COMM,IERROR)
  INTEGER(KIND=int4Kind), INTENT(INOUT), DIMENSION(:) :: BUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: ROOT
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPIBCastInt4(BUF      = BUF,      &
                    COUNT    = COUNT,    &
                    DATATYPE = DATATYPE, &
                    ROOT     = ROOT,     &
                    COMM     = COMM,     &
                    IERROR   = IERROR)

  RETURN
END SUBROUTINE MPI_BCASTInt4

END MODULE MPI_BCASTInt4Module
