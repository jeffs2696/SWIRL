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

MODULE MPI_BCASTReal8Call

  USE IncludeMPIImplementation
  USE MPIKindDefs

! this module actually makes the F77 MPI_BCAST call for real8 data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIBCastReal8

CONTAINS

SUBROUTINE MPIBCastReal8(BUF,COUNT,DATATYPE,ROOT,COMM,IERROR)
  REAL(KIND=real8Kind), INTENT(INOUT), DIMENSION(:) :: BUF
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
END SUBROUTINE MPIBCastReal8

END MODULE MPI_BCASTReal8Call

MODULE MPI_BCASTReal8Module
  USE MPI_BCASTReal8Call
  USE MPIKindDefs

! this module is the wrapper to make the new F95 MPI_BCAST
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: HTMPI_BCAST

INTERFACE HTMPI_BCAST
  MODULE PROCEDURE MPI_BCASTReal8
END INTERFACE HTMPI_BCAST

CONTAINS

SUBROUTINE MPI_BCASTReal8(BUF,COUNT,DATATYPE,ROOT,COMM,IERROR)
  REAL(KIND=real8Kind), INTENT(INOUT), DIMENSION(:) :: BUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: ROOT
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPIBCastReal8(BUF      = BUF,      &
                    COUNT    = COUNT,    &
                    DATATYPE = DATATYPE, &
                    ROOT     = ROOT,     &
                    COMM     = COMM,     &
                    IERROR   = IERROR)

  RETURN
END SUBROUTINE MPI_BCASTReal8

END MODULE MPI_BCASTReal8Module
