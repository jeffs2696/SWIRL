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

MODULE MPI_BCASTDblePrecCall

  USE IncludeMPIImplementation

! this module actually makes the F77 MPI_BCAST call for real8 data

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPIBCastDblePrec

CONTAINS

SUBROUTINE MPIBCastDblePrec(BUF,COUNT,DATATYPE,ROOT,COMM,IERROR)
  DOUBLE PRECISION, INTENT(INOUT), DIMENSION(:) :: BUF
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
END SUBROUTINE MPIBCastDblePrec

END MODULE MPI_BCASTDblePrecCall

MODULE MPI_BCASTDblePrecModule
  USE MPI_BCASTDblePrecCall

! this module is the wrapper to make the new F95 MPI_BCAST
!  call _look like_ the standard F77 call.

  IMPLICIT NONE
  PRIVATE
  PUBLIC :: MPI_BCAST

INTERFACE MPI_BCAST
  MODULE PROCEDURE MPI_BCASTDblePrec
END INTERFACE MPI_BCAST

CONTAINS

SUBROUTINE MPI_BCASTDblePrec(BUF,COUNT,DATATYPE,ROOT,COMM,IERROR)
  DOUBLE PRECISION, INTENT(INOUT), DIMENSION(:) :: BUF
  INTEGER, INTENT(IN)  :: COUNT
  INTEGER, INTENT(IN)  :: DATATYPE
  INTEGER, INTENT(IN)  :: ROOT
  INTEGER, INTENT(IN)  :: COMM
  INTEGER, INTENT(OUT) :: IERROR

  CALL MPIBCastDblePrec(BUF      = BUF,      &
                     COUNT    = COUNT,    &
                     DATATYPE = DATATYPE, &
                     ROOT     = ROOT,     &
                     COMM     = COMM,     &
                     IERROR   = IERROR)

  RETURN
END SUBROUTINE MPI_BCASTDblePrec

END MODULE MPI_BCASTDblePrecModule
